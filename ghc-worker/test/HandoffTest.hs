-- | Description: A request runs in the directory its client handed over as a descriptor, which reaches the client's
-- root even when the path the client sends names nothing on the server's side, as in a remote executor's container;
-- and a server started with @--jobs N@ offers N slot locks, so N directory-mode clients share it.
module HandoffTest where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, replicateM_, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Foreign.C.Types (CInt (..), CUInt (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import GhcWorker.Caps (slotLockPaths)
import GhcWorker.CwdHandoff (cwdSocketPath, lookupCwd, serveCwdHandoff)
import GhcWorker.RequestCwd (ProcessCwdLock (..), metadataInProcessCwd, newProcessCwdLock, requestCwdFdVar, requestCwdVar, withRequestCwd)
import Hedgehog (TestT, assert, evalEither, (===))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, defaultProtocol, sendFd, socket)
import Network.Socket.ByteString (recv)
import System.Directory (canonicalizePath, createDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))
import Test.Run (unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)
import Types.Grpc (CommandEnv (..))

handOver :: FilePath -> FilePath -> IO (String, Socket)
handOver server dir = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix (cwdSocketPath server))
  Fd fd <- openFd dir ReadOnly defaultFileFlags {directory = True}
  sendFd sock fd
  closeFd (Fd fd)
  line <- recvLine sock B.empty
  pure (line, sock)
  where
    recvLine sock acc = do
      chunk <- recv sock 64
      let acc' = acc <> chunk
      case BS.elemIndex '\n' acc' of
        Just i -> pure (BS.unpack (BS.take i acc'))
        Nothing -> recvLine sock acc'

withHandoffServer :: FilePath -> (ProcessCwdLock -> IO a) -> IO a
withHandoffServer server use = do
  lock@(ProcessCwdLock _ registry _) <- newProcessCwdLock
  bracket (forkIO (serveCwdHandoff registry (cwdSocketPath server))) killThread \ _ -> do
    waitFor (doesFileExist (cwdSocketPath server))
    use lock
  where
    waitFor cond = cond >>= \ ok -> if ok then pure () else threadDelay 10_000 >> waitFor cond

-- | The client sends a path the server cannot resolve, as a containerised action does, and the descriptor beside it.
-- The request must run in the handed-over directory, and the server must release the descriptor once the client
-- closes the handoff connection.
test_handedOver :: IO FilePath -> TestT IO ()
test_handedOver tmpResource = do
  tmp <- liftIO tmpResource
  let root = tmp </> "execroot-of-one-action"
      server = tmp </> "s0"
  liftIO (createDirectory root)
  expected <- liftIO (canonicalizePath root)
  (seen, released) <- liftIO $ withHandoffServer server \ lock@(ProcessCwdLock _ registry _) -> do
    (token, sock) <- handOver server root
    let env = CommandEnv (Map.fromList [(requestCwdFdVar, token), (requestCwdVar, "/buildbuddy-execroot")])
    seen <- withRequestCwd lock env False getCurrentDirectory
    close sock
    released <- poll 200 (null <$> lookupCwd registry token)
    pure (seen, released)
  seen === expected
  assert released
  where
    poll :: Int -> IO Bool -> IO Bool
    poll 0 _ = pure False
    poll n cond = cond >>= \ ok -> if ok then pure True else threadDelay 10_000 >> poll (n - 1) cond

-- | A token the server never gave out fails the request instead of running it in the server's own directory.
test_unknownToken :: IO FilePath -> TestT IO ()
test_unknownToken tmpResource = do
  tmp <- liftIO tmpResource
  result <- liftIO $ withHandoffServer (tmp </> "s1") \ lock -> do
    let env = CommandEnv (Map.fromList [(requestCwdFdVar, "999")])
    try @IOException (withRequestCwd lock env False getCurrentDirectory)
  case result of
    Left _ -> pure ()
    Right dir -> do
      _ <- evalEither (Left ("ran in " ++ dir) :: Either String ())
      pure ()

foreign import ccall safe "usleep"
  c_usleep :: CUInt -> IO CInt

-- | Two compile requests and three metadata requests from five roots race on
-- one server, each reading a relative file that names its root. A compile
-- request's safe foreign calls make the RTS start worker OS threads from its
-- bound thread, which share its private working directory; a metadata request
-- that changed the process's directory from an unbound thread moved those, and
-- read from whichever thread it migrated to (the shared server failed both
-- ways on mercury-web-backend). Every read must see its own root.
test_metadataRace :: IO FilePath -> TestT IO ()
test_metadataRace tmpResource = do
  tmp <- liftIO tmpResource
  wrong <- liftIO do
    lock <- newProcessCwdLock
    bad <- newIORef (0 :: Int)
    done <- newEmptyMVar
    let roots = ["A", "B", "M1", "M2", "M3"]
    forM_ roots \ r -> do
      createDirectory (tmp </> ("race-" ++ r))
      writeFile (tmp </> ("race-" ++ r) </> "marker") r
    let request r wide = do
          result <- try @IOException $ withRequestCwd lock (CommandEnv (Map.fromList [(requestCwdVar, tmp </> ("race-" ++ r))])) wide do
            replicateM_ 200 do
              _ <- c_usleep 20
              m <- readFile "marker"
              when (m /= r) (atomicModifyIORef' bad (\ n -> (n + 1, ())))
          either (const (atomicModifyIORef' bad (\ n -> (n + 1, ())))) pure result
        spawn act = forkIO (act >> putMVar done ())
    forM_ ["A", "B"] \ r -> spawn (replicateM_ 20 (request r False))
    forM_ ["M1", "M2", "M3"] \ r -> spawn (replicateM_ 60 (request r metadataInProcessCwd))
    replicateM_ (length roots) (takeMVar done)
    readIORef bad
  wrong === 0

test_slotLocks :: TestT IO ()
test_slotLocks = do
  slotLockPaths "/run/ghc-worker/s0" 1 === ["/run/ghc-worker/s0.lock"]
  slotLockPaths "/run/ghc-worker/s0" 3 === ["/run/ghc-worker/s0.lock", "/run/ghc-worker/s0.lock.1", "/run/ghc-worker/s0.lock.2"]
  slotLockPaths "/run/ghc-worker/s0" 0 === ["/run/ghc-worker/s0.lock"]

test_handoff :: TestTree
test_handoff =
  withTemp "handoff" \ tmp ->
    testGroup "working directory handed over as a descriptor" [
      unitTest "a request runs in the handed-over directory, which is released after" (test_handedOver tmp),
      unitTest "an unknown token fails the request" (test_unknownToken tmp),
      unitTest "a server with N slots offers N lock files" test_slotLocks,
      unitTest "metadata and compile requests from different roots keep their own directories" (test_metadataRace tmp)
    ]
