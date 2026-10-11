{-# LANGUAGE CPP #-}

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
-- The client names the directory in the environment entry @GHC_WORKER_CWD@ of
-- its @ExecuteCommand@; a request without it runs as before, in the server's
-- working directory. Linux only: @unshare@ is a Linux system call.
module GhcWorker.RequestCwd (
  ProcessCwdLock,
  newProcessCwdLock,
  requestCwdVar,
  withRequestCwd,
) where

import Control.Concurrent (runInBoundThread)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Map.Strict qualified as Map
import Types.Grpc (CommandEnv (..))

#if defined(linux_HOST_OS)

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import System.Directory (setCurrentDirectory)

#else

import Control.Exception (throwIO)
import System.Directory (setCurrentDirectory)

#endif

-- | Held by the request that has changed the process's working directory.
newtype ProcessCwdLock =
  ProcessCwdLock (MVar ())

newProcessCwdLock :: IO ProcessCwdLock
newProcessCwdLock = ProcessCwdLock <$> newMVar ()

-- | The environment entry in which a client names the directory its request's
-- relative paths are anchored at. The same literal is in the client's source.
requestCwdVar :: String
requestCwdVar = "GHC_WORKER_CWD"

-- | Run a request handler in the working directory the request names, if it
-- names one: in a thread of its own with a working directory of its own, or,
-- when the handler's work reaches threads the request does not own, as the
-- process's working directory, one such request at a time.
withRequestCwd :: ProcessCwdLock -> CommandEnv -> Bool -> IO a -> IO a
withRequestCwd (ProcessCwdLock lock) (CommandEnv env) processWide run =
  case Map.lookup requestCwdVar env of
    Nothing -> run
    Just cwd
      | processWide -> withMVar lock \ () -> setCurrentDirectory cwd >> run
      | otherwise -> runInBoundThread (inOwnCwd cwd run)

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

inOwnCwd :: FilePath -> IO a -> IO a
inOwnCwd cwd run = do
  throwErrnoIfMinus1_ "unshare(CLONE_FS)" (c_unshare cloneFs)
  setCurrentDirectory cwd
  run

#else

inOwnCwd :: FilePath -> IO a -> IO a
inOwnCwd cwd _ =
  throwIO (userError ("a request named " ++ requestCwdVar ++ "=" ++ cwd ++ ", which only a Linux server supports"))

#endif
