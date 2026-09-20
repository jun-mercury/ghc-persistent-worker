-- | A client for a running @ghc-worker@ whose command line is GHC's.
--
-- Where buck2 cannot run the worker protocol itself, an action's command is
-- this program: it sends its own arguments and environment to the server
-- listening on the unix socket @$GHC_PERSISTENT_WORKER_SOCKET@ as one
-- @ExecuteCommand@, prints the response's stderr, and exits with the
-- response's exit code. The environment gains @GHC_WORKER_CWD@, the client's
-- working directory, so the server resolves the request's relative paths
-- against the execution root this client runs in (see
-- "GhcWorker.RequestCwd", which reads the same literal).
--
-- @$GHC_PERSISTENT_WORKER_SOCKET@ may also name a directory. Then it holds
-- one socket per server process, and the client sends its request to a
-- server no other client is using: it takes an exclusive @fcntl@ lock on the
-- file @<socket>.lock@ beside a socket, the first it gets, and holds it until
-- it exits. Clients that find every server taken try again every 25 ms. This
-- is how a machine serves N requests at once when a server can serve one:
-- where the kernel refuses @unshare(CLONE_FS)@, a server has one working
-- directory for all its requests and runs them one at a time, so N servers
-- with @--jobs 1@ each stand in for one with @--jobs N@, and the clients
-- share them out.
--
-- Only the proto package and grapesy are linked, so the binary does not carry
-- the @ghc@ library the server does.
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, displayException, try)
import Data.ByteString.Char8 qualified as BS
import Data.List (sort)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.GRPC.Client (Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import BuckWorkerProto (ExecuteCommand, ExecuteCommand'EnvironmentEntry, ExecuteResponse, Worker)
import Proto.Worker_Fields qualified as Fields
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (SeekMode (..), hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isSocket)
import System.Posix.IO (LockRequest (..), OpenFileFlags (..), OpenMode (..), defaultFileFlags, openFd, setLock)
import System.Posix.Types (Fd)

socketVar :: String
socketVar = "GHC_PERSISTENT_WORKER_SOCKET"

requestCwdVar :: String
requestCwdVar = "GHC_WORKER_CWD"

entry :: (String, String) -> Proto ExecuteCommand'EnvironmentEntry
entry (key, value) =
  defMessage
    & Fields.key .~ BS.pack key
    & Fields.value .~ BS.pack value

request :: [String] -> [(String, String)] -> Proto ExecuteCommand
request argv env =
  defMessage
    & Fields.argv .~ (BS.pack <$> argv)
    & Fields.env .~ (entry <$> env)

execute :: FilePath -> Proto ExecuteCommand -> IO (Proto ExecuteResponse)
execute socket req =
  withConnection def (ServerUnix socket) \ connection ->
    withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
      sendFinalInput call req
      recvNextOutput call

-- | The sockets in a directory of servers, in name order.
serverSockets :: FilePath -> IO [FilePath]
serverSockets dir = do
  entries <- sort <$> listDirectory dir
  fmap concat $ traverse (\ e -> socketOnly (dir </> e)) entries
  where
    socketOnly path = do
      status <- try (getFileStatus path)
      pure case status of
        Right st | isSocket st -> [path]
        Right _ -> []
        Left (_ :: IOException) -> []

-- | An exclusive lock on the lock file beside a socket, if no other process
-- holds one. The lock lives as long as the descriptor, which is as long as
-- this process.
tryLockServer :: FilePath -> IO (Maybe Fd)
tryLockServer socket = do
  fd <- openFd (socket ++ ".lock") WriteOnly defaultFileFlags {creat = Just 0o644}
  locked <- try (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
  pure case locked of
    Right () -> Just fd
    Left (_ :: IOException) -> Nothing

-- | A socket of the directory's servers that no other client holds, waited
-- for as long as it takes; the lock is released when the process exits.
lockFreeServer :: FilePath -> IO FilePath
lockFreeServer dir = go (0 :: Int)
  where
    go tries = do
      sockets <- serverSockets dir
      if null sockets
      then fail' (dir ++ " holds no server socket")
      else do
        taken <- firstFree sockets
        case taken of
          Just socket -> pure socket
          Nothing -> do
            -- Every 25 ms; a compile takes seconds, so the delay is noise
            -- against it, and the poll costs one fcntl per server.
            threadDelay 25_000
            go (tries + 1)

    firstFree [] = pure Nothing
    firstFree (socket : rest) =
      tryLockServer socket >>= \case
        Just _fd -> pure (Just socket)
        Nothing -> firstFree rest

main :: IO ()
main = do
  argv <- getArgs
  socketOrDir <- lookupEnv socketVar >>= \case
    Just s | not (null s) -> pure s
    _ -> fail' (socketVar ++ " is not set; it names the unix socket of the ghc-worker to send this command to, or a directory of such sockets")
  isDir <- doesDirectoryExist socketOrDir
  socket <- if isDir then lockFreeServer socketOrDir else pure socketOrDir
  cwd <- getCurrentDirectory
  env <- filter ((/= requestCwdVar) . fst) <$> getEnvironment
  result <- try (execute socket (request argv ((requestCwdVar, cwd) : env)))
  case result of
    Left (e :: SomeException) ->
      fail' ("no response from the ghc-worker at " ++ socket ++ ": " ++ displayException e)
    Right response -> do
      let output = response.stderr
      if Text.null output then pure () else Text.hPutStr stderr output
      exitWith case response.exitCode of
        0 -> ExitSuccess
        code -> ExitFailure (fromIntegral code)

fail' :: String -> IO a
fail' msg = do
  hPutStrLn stderr ("ghc-worker-client: " ++ msg)
  exitWith (ExitFailure 1)
