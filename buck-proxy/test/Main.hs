-- | Lifecycle tests for buck-proxy: what happens to the ghc-worker servers it
-- spawned when buck2 stops it.
--
-- buck2 stops a persistent worker with SIGTERM and, if it is still there after
-- a grace period, SIGKILL, and it signals the worker's pid only. Whatever
-- buck-proxy leaves behind keeps its memory until something else kills it, and
-- a GHC server holds tens of gigabytes.
--
-- The test binary is also the fake server: started with @--serve PATH@ it
-- writes its pid beside PATH and answers every request on it, so the tests need
-- no GHC.
module Main (main) where

import BuckWorkerProto (ExecuteCommand, ExecuteResponse)
import Common.Grpc (runGrpcServer, sendRequest, streamingNotImplemented)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Network.GRPC.Client (Server (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.StreamType (Methods (..), mkClientStreaming, mkNonStreaming)
import Proto.Worker (Worker)
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removePathForcibly)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.IO (IOMode (WriteMode), hFlush, stdout, withFile)
import System.OsPath (encodeUtf)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (UseHandle), createProcess, getPid, getProcessExitCode, proc)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)

main :: IO ()
main =
  getArgs >>= \case
    ["--serve", path] -> fakeServer path
    _ -> runTests

-- | The stand-in for @ghc-worker --serve PATH@.
fakeServer :: FilePath -> IO ()
fakeServer path = do
  pid <- getProcessID
  writeFile (takeDirectory path </> "pid") (show pid)
  socket <- encodeUtf path
  runGrpcServer socket methods
  where
    methods :: Methods IO (ProtobufMethodsOf Worker)
    methods =
      Method (mkClientStreaming streamingNotImplemented) $
      Method (mkNonStreaming answer) $
      NoMoreMethods

    answer :: Proto ExecuteCommand -> IO (Proto ExecuteResponse)
    answer _ = pure (defMessage & Fields.exitCode .~ 0)

-- | What a test case does once buck-proxy runs and has spawned its server.
data Case =
  Case {
    name :: String,
    remain :: Bool,
    linuxOnly :: Bool,
    -- | Stop buck-proxy, and say what went wrong, if anything.
    check :: ProcessHandle -> ProcessID -> IO [String]
  }

cases :: [Case]
cases =
  [
    Case {
      name = "SIGTERM: buck-proxy kills its server and exits",
      remain = False,
      linuxOnly = False,
      check = \ proxy server -> do
        signal sigTERM proxy
        exited <- waitExit proxy 10
        gone <- waitGone server 10
        pure $
          ["buck-proxy is still running 10 s after SIGTERM" | not exited] ++
          ["its server is still running 10 s after SIGTERM" | not gone]
    },
    Case {
      name = "SIGKILL: the server dies with buck-proxy",
      remain = False,
      linuxOnly = True,
      check = \ proxy server -> do
        signal sigKILL proxy
        _ <- waitExit proxy 10
        gone <- waitGone server 10
        pure ["the server outlived a SIGKILLed buck-proxy by 10 s" | not gone]
    },
    Case {
      name = "--remain: buck-proxy exits on SIGTERM and leaves its server running",
      remain = True,
      linuxOnly = False,
      check = \ proxy server -> do
        signal sigTERM proxy
        exited <- waitExit proxy 10
        threadDelay 1_000_000
        alive <- isAlive server
        when alive (signalProcess sigKILL server)
        pure $
          ["buck-proxy is still running 10 s after SIGTERM" | not exited] ++
          ["--remain did not keep the server" | not alive]
    }
  ]

runTests :: IO ()
runTests = do
  self <- getExecutablePath
  proxyExe <- findProxy self
  failures <- newIORef (0 :: Int)
  for' (zip [1 :: Int ..] cases) \ (n, c) ->
    if c.linuxOnly && os /= "linux"
    then putStrLn ("SKIP " ++ c.name ++ " (Linux only)")
    else runCase failures proxyExe self n c
  count <- readIORef failures
  unless (count == 0) do
    putStrLn (show count ++ " of " ++ show (length cases) ++ " cases failed")
    exitFailure
  where
    for' xs f = mapM_ f xs

-- | buck-proxy from @BUCK_PROXY_EXE@, the @PATH@ (cabal's build-tool-depends), or beside this test in a Setup build
-- tree.
findProxy :: FilePath -> IO FilePath
findProxy self =
  lookupEnv "BUCK_PROXY_EXE" >>= \case
    Just exe -> pure exe
    Nothing ->
      findExecutable "buck-proxy" >>= \case
        Just exe -> pure exe
        Nothing -> pure (takeDirectory (takeDirectory self) </> "buck-proxy" </> "buck-proxy")

runCase :: IORef Int -> FilePath -> FilePath -> Int -> Case -> IO ()
runCase failures proxyExe self n c = do
  me <- getProcessID
  let tag = "bpt" ++ show me ++ "c" ++ show n
      -- buck-proxy reads the trace id from the fourth component, as in buck2's
      -- /tmp/buck2_worker/{uuid}-{number}/socket.
      base = "/tmp" </> tag
      workerSocket = base </> (tag ++ "-1") </> "socket"
      buildId = tag ++ "build"
      targetId = "target"
      serverDir = "/tmp/ghc-persistent-worker" </> (buildId ++ "_" ++ targetId)
  removePathForcibly base
  removePathForcibly serverDir
  createDirectoryIfMissing True (takeDirectory workerSocket)
  env <- getEnvironment
  let logFile = base </> "buck-proxy.log"
  problems <-
    withFile logFile WriteMode \ logHandle -> do
      -- buck-proxy and its server write to a file rather than to this test's
      -- output: a process that outlives the test would otherwise hold the
      -- test runner's pipe open, and the runner would wait on it forever.
      let process =
            (proc proxyExe (["--exe", self] ++ ["--remain" | c.remain]))
              {
                env = Just (("WORKER_SOCKET", workerSocket) : filter ((/= "WORKER_SOCKET") . fst) env),
                std_out = UseHandle logHandle,
                std_err = UseHandle logHandle
              }
      bracket (createProcess process) (\ (_, _, _, ph) -> cleanUp ph (serverDir </> "pid")) \ (_, _, _, proxy) -> do
        reached <- request workerSocket buildId targetId
        if not reached
        then pure ["buck-proxy did not answer a request within 10 s"]
        else
          readPid (serverDir </> "pid") >>= \case
            Nothing -> pure ["buck-proxy spawned no server"]
            Just server -> c.check proxy server
  unless (null problems) do
    output <- readFile logFile
    mapM_ (putStrLn . ("  buck-proxy: " ++)) (lines output)
  removePathForcibly base
  removePathForcibly serverDir
  if null problems
  then putStrLn ("PASS " ++ c.name)
  else do
    modifyIORef' failures (+ 1)
    putStrLn ("FAIL " ++ c.name)
    mapM_ (putStrLn . ("  " ++)) problems
  hFlush stdout

-- | Send one request through buck-proxy, retrying while its socket comes up.
request :: FilePath -> String -> String -> IO Bool
request socket buildId targetId = go (100 :: Int)
  where
    go 0 = pure False
    go k =
      try (sendRequest (ServerUnix socket) command) >>= \case
        Right _ -> pure True
        Left (_ :: SomeException) -> threadDelay 100_000 >> go (k - 1)

    command :: Proto ExecuteCommand
    command =
      defMessage
        & Fields.argv .~ ["--worker-target-id", ByteString.pack targetId]
        & Fields.env .~ [defMessage & Fields.key .~ "BUCK_BUILD_ID" & Fields.value .~ ByteString.pack buildId]

-- | Whatever a case left running goes, so that one failure cannot hang the suite.
cleanUp :: ProcessHandle -> FilePath -> IO ()
cleanUp proxy pidFile = do
  signal sigKILL proxy
  readPid pidFile >>= \case
    Just server -> do
      alive <- isAlive server
      when alive (signalProcess sigKILL server)
    Nothing -> pure ()

readPid :: FilePath -> IO (Maybe ProcessID)
readPid path = do
  exists <- doesFileExist path
  if exists then Just . read <$> readFile path else pure Nothing

signal :: Signal -> ProcessHandle -> IO ()
signal sig ph =
  getPid ph >>= \case
    Just pid -> signalProcess sig pid
    Nothing -> pure ()

-- | Wait up to the given number of seconds for buck-proxy to exit.
waitExit :: ProcessHandle -> Int -> IO Bool
waitExit ph seconds = poll (seconds * 10)
  where
    poll 0 = isJust <$> getProcessExitCode ph
    poll k =
      getProcessExitCode ph >>= \case
        Just _ -> pure True
        Nothing -> threadDelay 100_000 >> poll (k - 1)

-- | Wait up to the given number of seconds for a process that is not our child to go.
waitGone :: ProcessID -> Int -> IO Bool
waitGone pid seconds = poll (seconds * 10)
  where
    poll 0 = not <$> isAlive pid
    poll k =
      isAlive pid >>= \case
        False -> pure True
        True -> threadDelay 100_000 >> poll (k - 1)

-- | Whether a process runs. A zombie does not: a server orphaned and killed waits for a reaper that may never come
-- inside a sandbox, and its memory is already free.
isAlive :: ProcessID -> IO Bool
isAlive pid
  | os == "linux" = do
      result <- try (readFile ("/proc/" ++ show pid ++ "/stat"))
      pure case result of
        Left (_ :: IOError) -> False
        Right stat -> case words (drop 1 (dropWhile (/= ')') stat)) of
          state : _ -> state /= "Z" && state /= "X"
          [] -> False
  | otherwise = isRight <$> try @IOError (signalProcess 0 pid)
  where
    isRight = either (const False) (const True)
