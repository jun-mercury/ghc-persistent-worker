-- | Description: A client lists only server sockets in a directory of servers. The directory is laid out the way the
-- BuildBuddy sidecar lays it out (@<lane>-<generation>@ with its @.lock@, @.lock.<i>@ and @.cwd@ siblings), with real
-- listening sockets, since the handoff socket being a socket is what made it look like a server.
module SocketsTest where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import GhcWorkerClient.Sockets (byBinding, isServerSocketName, readBinding, serverSockets)
import Hedgehog (TestT, assert, (===))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), bind, close, defaultProtocol, listen, socket)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSocket)
import Test.Run (unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)

listening :: FilePath -> IO Socket
listening path = do
  s <- socket AF_UNIX Stream defaultProtocol
  bind s (SockAddrUnix path)
  listen s 1
  pure s

test_sidecarLayout :: IO FilePath -> TestT IO ()
test_sidecarLayout tmpResource = do
  tmp <- liftIO tmpResource
  let dir = tmp </> "ghckey"
      server0 = dir </> "0-3"
      server1 = dir </> "1-1"
  (handoffIsSocket, sockets) <- liftIO do
    createDirectory dir
    bracket (traverse @[] listening [server0, server0 ++ ".cwd", server1, server1 ++ ".cwd"]) (mapM_ close) \ _ -> do
      mapM_ @[] (\ f -> writeFile f "") [server0 ++ ".lock", server0 ++ ".lock.1", server0 ++ ".lock.2", server1 ++ ".lock", server1 ++ ".build", server1 ++ ".build.tmp", dir </> "notes"]
      (,) <$> (isSocket <$> getFileStatus (server0 ++ ".cwd")) <*> serverSockets dir
  assert handoffIsSocket
  sockets === [server0, server1]

-- | A server bound to the client's build first, unbound ones next, and those bound to another build last, each group in
-- the listing's order.
test_byBinding :: TestT IO ()
test_byBinding =
  (fst <$> byBinding "b" [("0-1", Just "a"), ("1-1", Nothing), ("2-1", Just "b"), ("3-1", Just "a"), ("4-1", Nothing), ("5-1", Just "b")])
    === ["2-1", "5-1", "1-1", "4-1", "0-1", "3-1"]

test_readBinding :: IO FilePath -> TestT IO ()
test_readBinding tmpResource = do
  tmp <- liftIO tmpResource
  let bound = tmp </> "bound"
  (key, none) <- liftIO do
    writeFile (bound ++ ".build") "inv-1"
    (,) <$> readBinding bound <*> readBinding (tmp </> "unbound")
  key === Just "inv-1"
  none === Nothing

test_names :: TestT IO ()
test_names = do
  assert (isServerSocketName "0-3")
  assert (isServerSocketName "s0")
  assert (not (isServerSocketName "0-3.cwd"))
  assert (not (isServerSocketName "0-3.lock"))
  assert (not (isServerSocketName "0-3.lock.7"))
  assert (not (isServerSocketName "0-3.cwd.lock"))

test_sockets :: TestTree
test_sockets =
  withTemp "sockets" \ tmp ->
    testGroup "a client lists only server sockets" [
      unitTest "the sidecar's layout yields its servers, never a handoff socket" (test_sidecarLayout tmp),
      unitTest "handoff and lock names are not servers" test_names,
      unitTest "a client tries its own build's servers, then unbound ones, then the rest" test_byBinding,
      unitTest "a binding file names the build, and its absence none" (test_readBinding tmp)
    ]
