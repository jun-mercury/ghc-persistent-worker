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
-- A server comes and goes under the clients: it retires itself past a cap
-- (see "GhcWorker.Caps") and the boot script's loop starts another on the
-- same path a second later, and the kernel's OOM killer took one on
-- 2026-09-21 and left its socket file behind. So a socket the client locked
-- but cannot connect to is released and skipped, an empty directory or one
-- whose every socket is dead is retried for up to two minutes and then fails
-- the action with a message, rather than a hang buck2 and the executor never
-- time out, and a connection that drops after the request was sent fails the
-- action at once: the response is not coming, and a retry is buck2's.
--
-- The directory is routed before it is searched. @$GHC_WORKER_GHC_KEY@ names
-- the compiler, the store hash of the GHC the action would have run, and picks
-- a subdirectory: a server holds the session of one compiler, so two
-- toolchains on one machine keep separate servers.
--
-- @$GHC_WORKER_BUILD_KEY@ names the build, and the server, not the directory,
-- holds it: a server serves the build of its first request and refuses any
-- other with exit 77, then retires (see "GhcWorker.BuildKey" in the server).
-- The client sends the key with the rest of its environment, tries first the
-- servers bound to its build, then unbound ones, then the rest, least recently
-- used first. After one
-- refusal it waits for a server its build may use, the refusing server's
-- successor among them, rather than refuse its way through every server
-- another build warmed. It once picked a subdirectory per build
-- instead, which only worked where something started servers per build, and
-- nothing did.
--
-- The exit code tells the harness why an action that never compiled failed:
-- 75 when no server answered (no socket, or none alive within two minutes),
-- 76 when the server took the request and the connection dropped before the
-- response, as when the server was killed mid-compile. Neither is a compile
-- error, and neither is retried here; a retry is buck2's. The server's own 78
-- passes through: it requires a build key and the action set none.
--
-- A server started with @--jobs N@ serves N requests at once and has a lock
-- file per slot, @<socket>.lock@ and @<socket>.lock.1@ to @.lock.<N-1>@; the
-- client takes the first free one, so N clients share that server.
--
-- Before sending its request, the client hands the server its working
-- directory as an open descriptor over @<socket>.cwd@, when the server has
-- that socket, and names the token it gets back in @GHC_WORKER_CWD_FD@: on a
-- remote executor the action's root has a path the server cannot see (see
-- "GhcWorker.CwdHandoff" in the server). The handoff connection stays open
-- until the client exits, which is how the server knows to close the
-- descriptor.
--
-- Each request prints one line to stderr, the socket it went to, the build
-- key and how long it waited for a free server, so the stress test can count
-- which builds each server served.
--
-- Only the proto package and grapesy are linked, so the binary does not carry
-- the @ghc@ library the server does.
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, displayException, fromException, try)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.ByteString qualified as B
import Foreign.C.Error (Errno (..), eCONNREFUSED, eNOENT)
import GHC.Clock (getMonotonicTime)
import GHC.IO.Exception (IOException (..))
import Network.GRPC.Client (Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import BuckWorkerProto (ExecuteCommand, ExecuteCommand'EnvironmentEntry, ExecuteResponse, Worker)
import GhcWorkerClient.Sockets (Binding (..), byBinding, readBinding, serverSockets)
import Proto.Worker_Fields qualified as Fields
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), close, connect, defaultProtocol, sendFd, socket)
import Network.Socket.ByteString (recv)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (SeekMode (..), hPutStrLn, stderr)
import System.Posix.IO (LockRequest (..), OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd, setLock)
import System.Posix.Types (Fd (..))

socketVar :: String
socketVar = "GHC_PERSISTENT_WORKER_SOCKET"

requestCwdVar :: String
requestCwdVar = "GHC_WORKER_CWD"

requestCwdFdVar :: String
requestCwdFdVar = "GHC_WORKER_CWD_FD"

ghcKeyVar :: String
ghcKeyVar = "GHC_WORKER_GHC_KEY"

buildKeyVar :: String
buildKeyVar = "GHC_WORKER_BUILD_KEY"

-- | Exit codes of an action that never got a compile's result, apart from the
-- compiler's own 1.
noServerExit, serverLostExit :: Int
noServerExit = 75
serverLostExit = 76

-- | A server's refusal of a request for another build than the one it holds.
wrongBuildExit :: Int
wrongBuildExit = 77

-- | The directory of servers for a compiler.
routedDirectory :: FilePath -> Maybe String -> FilePath
routedDirectory root = \case
  Just k | not (null k) -> root </> k
  _ -> root

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

-- | An exclusive lock on the lock file beside a socket, if no other process
-- holds one. The lock lives as long as the descriptor, which is as long as
-- this process unless the socket turns out dead and the descriptor is closed.
tryLockServer :: FilePath -> IO (Maybe Fd)
tryLockServer socket = go (0 :: Int)
  where
    go i = do
      let file = if i == 0 then socket ++ ".lock" else socket ++ ".lock." ++ show i
      exists <- doesFileExist file
      if i > 0 && not exists
      then pure Nothing
      else do
        fd <- openFd file WriteOnly defaultFileFlags {creat = Just 0o644}
        locked <- try (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
        case locked of
          Right () -> pure (Just fd)
          Left (_ :: IOException) -> do
            closeFd fd
            go (i + 1)

-- | Hand the server this process's working directory, if the server takes handoffs: the environment entry naming it,
-- and the connection, which must stay open until the response is in.
handOffCwd :: FilePath -> IO (Maybe ((String, String), Socket))
handOffCwd server = do
  let path = server ++ ".cwd"
  exists <- doesFileExist path
  if not exists then pure Nothing else do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix path)
    Fd dirFd <- openFd "." ReadOnly defaultFileFlags {directory = True}
    sendFd sock dirFd
    closeFd (Fd dirFd)
    token <- readLine sock B.empty
    pure (Just ((requestCwdFdVar, token), sock))
  where
    readLine sock acc = do
      chunk <- recv sock 64
      let acc' = acc <> chunk
      case BS.elemIndex '\n' acc' of
        Just i -> pure (BS.unpack (BS.take i acc'))
        Nothing
          | B.null chunk -> ioError (userError ("ghc-worker-client: " ++ path ++ " closed before naming a token"))
          | otherwise -> readLine sock acc'
      where
        path = server ++ ".cwd"

-- | Send the request to one server, after handing it the working directory when it takes handoffs.
executeAt :: FilePath -> ([(String, String)] -> Proto ExecuteCommand) -> IO (Proto ExecuteResponse)
executeAt server mkReq = do
  handed <- handOffCwd server
  response <- execute server (mkReq (maybe [] (pure . fst) handed))
  -- Closing here, once the response is in, is what tells the server to release the descriptor, and keeps the
  -- connection reachable until then.
  for_ handed (close . snd)
  pure response

-- | The response from a server of the directory that no other client holds,
-- waited for as long as it takes while some server is busy, and for at most
-- 'deadSeconds' while none answers.
requestFromDirectory :: FilePath -> String -> ([(String, String)] -> Proto ExecuteCommand) -> IO (FilePath, Double, Proto ExecuteResponse)
requestFromDirectory dir buildKey mkReq = do
  started <- getMonotonicTime
  go started False
  where
    -- @evicted@ once a server bound to another build has refused this request
    -- and is retiring: the client then waits for that server's successor, or
    -- for any server its build may use, rather than evicting another, so one
    -- request takes at most one server from another build.
    go started evicted = do
      servers <- byBinding buildKey <$> (traverse (\ s -> (s,) <$> readBinding s) =<< serverSockets dir)
      outcome <- tryEach started evicted servers False
      case outcome of
        Answered answered -> pure answered
        Evicted -> poll started True
        Busy -> poll started evicted
        AllDead -> do
          now <- getMonotonicTime
          if now - started > deadSeconds
          then failWith noServerExit (dir ++ " holds no server that answers" ++ (if null servers then " (no socket in it)" else "") ++ " after " ++ show (round deadSeconds :: Int) ++ " s")
          else poll started evicted

    -- Every 25 ms; a compile takes seconds, so the delay is noise against
    -- it, and the poll costs one fcntl per server.
    poll started evicted = do
      threadDelay 25_000
      go started evicted

    tryEach _ _ [] busy = pure (if busy then Busy else AllDead)
    tryEach started evicted ((socket, binding) : rest) busy
      | evicted, Just b <- binding, b.key /= buildKey = tryEach started evicted rest True
      | otherwise =
      tryLockServer socket >>= \case
        Nothing -> tryEach started evicted rest True
        Just fd -> do
          locked <- getMonotonicTime
          sent <- attempt socket
          case sent of
            Right response
              -- Bound to another build, and now retiring; its successor binds afresh.
              | fromIntegral response.exitCode == wrongBuildExit -> do
                closeFd fd
                pure Evicted
              | otherwise -> pure (Answered (socket, locked - started, response))
            Left ConnectFailed -> do
              closeFd fd
              tryEach started evicted rest busy

    attempt socket = do
      result <- try (executeAt socket mkReq)
      case result of
        Right response -> pure (Right response)
        Left (e :: SomeException)
          | isConnectFailure e -> pure (Left ConnectFailed)
          | otherwise -> failWith serverLostExit ("the connection to the ghc-worker at " ++ socket ++ " was lost before it answered: " ++ displayException e)

    deadSeconds :: Double
    deadSeconds = 120

data Outcome = Answered (FilePath, Double, Proto ExecuteResponse) | Evicted | Busy | AllDead

data ConnectFailed = ConnectFailed

-- | The failure of a connect to a socket nobody serves: the file is gone
-- (@ENOENT@) or nothing listens on it (@ECONNREFUSED@). grapesy rethrows the
-- connect's own IOException when the first call on the connection finds it
-- abandoned, so the errno is the one @connect(2)@ set.
isConnectFailure :: SomeException -> Bool
isConnectFailure e =
  case fromException e of
    Just IOError {ioe_errno = Just errno} -> Errno errno == eCONNREFUSED || Errno errno == eNOENT
    _ -> False

main :: IO ()
main = do
  argv <- getArgs
  socketOrDir <- lookupEnv socketVar >>= \case
    Just s | not (null s) -> pure s
    _ -> failWith noServerExit (socketVar ++ " is not set; it names the unix socket of the ghc-worker to send this command to, or a directory of such sockets")
  ghcKey <- lookupEnv ghcKeyVar
  buildKey <- lookupEnv buildKeyVar
  isDir <- doesDirectoryExist socketOrDir
  cwd <- getCurrentDirectory
  env <- filter (\ (key, _) -> key /= requestCwdVar && key /= requestCwdFdVar) <$> getEnvironment
  let mkReq extra = request argv ((requestCwdVar, cwd) : extra ++ env)
  (socket, waited, response) <-
    if isDir
    then requestFromDirectory (routedDirectory socketOrDir ghcKey) (maybe "" id buildKey) mkReq
    else try (executeAt socketOrDir mkReq) >>= \case
      Left (e :: SomeException)
        | isConnectFailure e -> failWith noServerExit ("no ghc-worker listens at " ++ socketOrDir ++ ": " ++ displayException e)
        | otherwise -> failWith serverLostExit ("the connection to the ghc-worker at " ++ socketOrDir ++ " was lost before it answered: " ++ displayException e)
      Right response -> pure (socketOrDir, 0, response)
  hPutStrLn stderr (requestLine socket buildKey waited)
  let output = response.stderr
  if Text.null output then pure () else Text.hPutStr stderr output
  exitWith case response.exitCode of
    0 -> ExitSuccess
    code -> ExitFailure (fromIntegral code)

-- | The line each request prints, for the harness that counts which builds a
-- server served and how long clients waited for one.
requestLine :: FilePath -> Maybe String -> Double -> String
requestLine socket buildKey waited =
  unwords ["ghc-worker-client: server", socket, "build", maybe "-" id buildKey, "wait_ms", show (round (waited * 1000) :: Int)]

failWith :: Int -> String -> IO a
failWith code msg = do
  hPutStrLn stderr ("ghc-worker-client: " ++ msg)
  exitWith (ExitFailure code)
