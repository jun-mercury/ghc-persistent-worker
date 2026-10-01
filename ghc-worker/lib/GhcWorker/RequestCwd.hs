{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | A working directory per request.
--
-- Every path in a request is relative to the project root of the client that
-- sent it: the source file, @-odir@ and @-hidir@, the @--ghc-args@ file, the
-- build plan, the dependency files, and the paths GHC keeps in the module graph
-- and the finder cache between requests. With buck2 running the server, that
-- root is the process's working directory and every client shares it. A server
-- started on a remote-execution machine serves actions that each run in an
-- execution root of their own, with the same layout below different roots.
--
-- Rewriting every path per request would mean re-anchoring the GHC structures
-- the requests share (the module graph, the finder cache, the unit state), so
-- instead the working directory itself is made per request: the handler runs
-- in a bound OS thread that first calls @unshare(CLONE_FS)@, which gives that
-- thread a working directory of its own, and then changes it to the directory
-- the client named. Every relative path the request touches, including the
-- ones cached from earlier requests, then resolves against the client's root,
-- and requests from different roots run concurrently without touching each
-- other's. A bound thread runs its Haskell code and its foreign calls on that
-- one OS thread, and a process it forks (the C compiler for a stub, say)
-- inherits that thread's working directory.
--
-- GHC's downsweep is the exception: it checks and parses the root files on
-- threads of its own, which a bound thread's working directory does not
-- reach. A metadata request therefore changes the process's working
-- directory instead, and metadata requests run one at a time under
-- 'ProcessCwdLock' so that no two roots are current at once; threads that
-- have called @unshare@ are unaffected by that change, so compile requests
-- keep running concurrently.
--
-- Where the kernel or a seccomp profile refuses @unshare@ (a container's
-- default profile often answers @EPERM@), a compile request takes the same
-- route as a metadata request, one at a time under the lock, and says so on
-- the server's stderr.
--
-- The client names the directory in the environment entry @GHC_WORKER_CWD@ of
-- its @ExecuteCommand@; a request without it runs as before, in the server's
-- working directory. Linux only: @unshare@ is a Linux system call.
--
-- A path names the client's directory only where client and server share a
-- mount namespace. On a remote executor that runs each action in a container
-- of its own, the action sees its execution root at a fixed path such as
-- @/buildbuddy-execroot@, while the server, in a sidecar, sees every root
-- under the executor's work directory, so the path is the same for every
-- action and names none of them. The client therefore also hands the server
-- an open descriptor of its working directory over the server's handoff
-- socket (see "GhcWorker.CwdHandoff") and names it with the token in
-- @GHC_WORKER_CWD_FD@; the server changes to that directory with @fchdir@,
-- which reaches it whatever the path is called on either side. The path stays
-- the fallback for a server without a handoff socket.
--
-- A descriptor from an action's container names the directory through that
-- container's mounts. After @fchdir@ to it, relative paths resolve, but
-- @getcwd@ fails with @ENOENT@: the kernel cannot reach the directory from the
-- server's root, since the container has a root of its own. GHC asks for the
-- working directory during a compile, so every such request failed with
-- "Current working directory no longer exists". The executor's work directory
-- is mounted in the server's container too, so the server looks below the
-- directories named in @GHC_WORKER_EXECROOTS@ (colon-separated, two levels
-- down) for the one with the descriptor's device and inode, and changes to
-- it by its own path. Only when none matches does it fall back to @fchdir@.
module GhcWorker.RequestCwd (
  ProcessCwdLock (..),
  newProcessCwdLock,
  requestCwdVar,
  requestCwdFdVar,
  withRequestCwd,
) where

import Control.Concurrent (runInBoundThread)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, displayException, finally, throwIO, try)
import Data.Map.Strict qualified as Map
import GhcWorker.CwdHandoff (CwdRegistry, lookupCwd, newCwdRegistry)
import System.Directory (listDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Posix.Files (FileStatus, deviceID, fileID, getFdStatus, getFileStatus, isDirectory)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))
import Types.Grpc (CommandEnv (..))

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))

-- | Held by the request that has changed the process's working directory, with the directories clients handed over
-- and the directory the server started in.
data ProcessCwdLock =
  ProcessCwdLock (MVar ()) CwdRegistry Fd

newProcessCwdLock :: IO ProcessCwdLock
newProcessCwdLock = ProcessCwdLock <$> newMVar () <*> newCwdRegistry <*> openFd "." ReadOnly defaultFileFlags {directory = True}

-- | The environment entry in which a client names the directory its request's
-- relative paths are anchored at. The same literal is in the client's source.
requestCwdVar :: String
requestCwdVar = "GHC_WORKER_CWD"

-- | The environment entry in which a client names the directory it handed over, by the token the server gave it.
requestCwdFdVar :: String
requestCwdFdVar = "GHC_WORKER_CWD_FD"

-- | How a request names its directory: a descriptor it handed over, or a path.
data RequestDir = HandedOver Fd | ByPath FilePath

-- | Run a request handler in the working directory the request names, if it
-- names one: in a thread of its own with a working directory of its own, or,
-- when the handler's work reaches threads the request does not own or the
-- thread cannot get a directory of its own, as the process's working
-- directory, one such request at a time.
withRequestCwd :: ProcessCwdLock -> CommandEnv -> Bool -> IO a -> IO a
withRequestCwd (ProcessCwdLock lock registry home) (CommandEnv env) processWide run = do
  dir <- case (Map.lookup requestCwdFdVar env, Map.lookup requestCwdVar env) of
    (Just token, _) ->
      lookupCwd registry token >>= \case
        Just fd -> pure (Just (HandedOver fd))
        Nothing -> throwIO (userError ("ghc-worker: no directory was handed over under " ++ requestCwdFdVar ++ "=" ++ token))
    (Nothing, path) -> pure (ByPath <$> path)
  case dir of
    Nothing -> run
    Just d
      | processWide -> inProcessCwd d
      | otherwise ->
          runInBoundThread do
            unshared <- try unshareFs
            case unshared of
              -- The RTS starts worker OS threads from whichever thread needs one, a bound thread in a safe foreign
              -- call included, and a thread started with pthread_create shares its creator's working directory. A
              -- worker started during this request keeps this request's private directory after it ends and goes
              -- on running other Haskell threads there, so the request leaves that directory as the server's own.
              Right () -> (changeTo d >> run) `finally` fchdir home
              Left (e :: IOException) -> do
                hPutStrLn stderr ("ghc-worker: " ++ displayException e ++ "; the request runs in the process's working directory instead")
                inProcessCwd d
  where
    inProcessCwd d = withMVar lock \ () -> changeTo d >> run

    changeTo = \case
      HandedOver fd -> reachablePath fd >>= maybe (fchdir fd) setCurrentDirectory
      ByPath path -> setCurrentDirectory path

-- | The path, in this process's mounts, of the directory a descriptor names:
-- the entry with the descriptor's device and inode up to two levels below a
-- directory in @GHC_WORKER_EXECROOTS@, if any is.
reachablePath :: Fd -> IO (Maybe FilePath)
reachablePath fd = do
  roots <- maybe [] (filter (not . null) . splitColons) <$> lookupEnv execrootsVar
  if null roots
    then pure Nothing
    else do
      st <- getFdStatus fd
      let key = (deviceID st, fileID st)
          matches path = (try (getFileStatus path) :: IO (Either IOException FileStatus)) >>= \case
            Right s | isDirectory s -> pure (Just ((deviceID s, fileID s) == key))
            _ -> pure Nothing
          children dir = either (const []) (map (\n -> dir ++ "/" ++ n)) <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
          search _ [] = pure Nothing
          search depth (dir : rest) = matches dir >>= \case
            Just True -> pure (Just dir)
            Just False | depth > 0 -> children dir >>= search (depth - 1) >>= maybe (search depth rest) (pure . Just)
            _ -> search depth rest
      roots' <- concat <$> mapM children roots
      search (1 :: Int) roots'
  where
    splitColons str = case break (== ':') str of
      (a, []) -> [a]
      (a, _ : b) -> a : splitColons b

-- | The environment entry, of the server's own environment, naming the
-- directories below which the executor keeps its actions' execution roots,
-- @/buildbuddy/remotebuilds@ on a BuildBuddy executor.
execrootsVar :: String
execrootsVar = "GHC_WORKER_EXECROOTS"

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt

fchdir :: Fd -> IO ()
fchdir (Fd fd) = throwErrnoIfMinus1_ "fchdir" (c_fchdir fd)

#if defined(linux_HOST_OS)

-- | @unshare(2)@; declared in @sched.h@ behind @_GNU_SOURCE@, so it is imported
-- by name rather than through the header.
foreign import ccall unsafe "unshare"
  c_unshare :: CInt -> IO CInt

-- | @CLONE_FS@ from @linux/sched.h@: the calling thread stops sharing its
-- root directory, working directory and umask with the rest of the process.
-- A kernel ABI constant, stable since Linux 2.6.16.
cloneFs :: CInt
cloneFs = 0x00000200

unshareFs :: IO ()
unshareFs = throwErrnoIfMinus1_ "unshare(CLONE_FS)" (c_unshare cloneFs)

#else

unshareFs :: IO ()
unshareFs = throwIO (userError "a thread of its own with a working directory of its own needs Linux's unshare(CLONE_FS)")

#endif
