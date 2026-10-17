{-# LANGUAGE CPP #-}

-- | Starting GHC servers so that they die with buck-proxy.
--
-- buck2 runs buck-proxy in a process group of its own and signals only its pid, and a SIGKILL, or a crash, leaves
-- no chance to clean up. A server outliving buck-proxy keeps its heap, tens of gigabytes, until something else
-- kills it. On Linux each server is therefore started with a parent-death signal (@PR_SET_PDEATHSIG@, prctl(2)):
-- the kernel sends it SIGKILL when buck-proxy goes, however it goes.
--
-- The @process@ library has no hook between fork and exec, so the server is started through buck-proxy itself:
-- @buck-proxy 'parentDeathFlag' PPID EXE ARGS...@ sets the signal, checks that its parent is still PPID (it may have
-- died before the signal was set), and execs EXE. The pid and the 'ProcessHandle' are the server's, since exec keeps
-- them.
--
-- The kernel sends the signal when the /thread/ that forked the child exits, not the process (prctl(2)), and the
-- threaded RTS retires idle OS threads. So every server is forked from one bound thread that lives as long as
-- buck-proxy.
module BuckProxy.Spawn (
  Spawner,
  newSpawner,
  spawn,
  parentDeathFlag,
  execDyingWithParent,
) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever, join, void)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile, exitImmediately, getParentProcessID, getProcessID)
import System.Posix.Types (ProcessID)
import System.Process (ProcessHandle, spawnProcess)
import Text.Read (readMaybe)

#if defined(linux_HOST_OS)
import Foreign.C.Types (CInt (..), CULong (..))
import System.Posix.Signals (sigKILL)
#endif

-- | Starts GHC server processes.
data Spawner =
  -- | A server stops only when buck-proxy stops it.
  Plain
  |
  -- | A server dies with buck-proxy.
  DyingWithParent {
    -- | buck-proxy's own executable.
    self :: FilePath,
    parent :: ProcessID,
    -- | Requests for the forking thread.
    requests :: MVar (IO ())
  }

-- | A 'Spawner' whose servers die with this process, or, with 'False' (@--remain@), outlive it.
newSpawner :: Bool -> IO Spawner
newSpawner dieWithParent
  | dieWithParent && parentDeathSupported = do
    self <- getExecutablePath
    parent <- getProcessID
    requests <- newEmptyMVar
    -- The 'Spawner' keeps @requests@ reachable for as long as anything can still spawn, so this thread is not
    -- collected as deadlocked while buck-proxy serves.
    void $ forkOS $ forever $ join (takeMVar requests)
    pure DyingWithParent {self, parent, requests}
  | otherwise =
    pure Plain

-- | Start @exe@ with @args@, as 'spawnProcess' does.
spawn :: Spawner -> FilePath -> [String] -> IO ProcessHandle
spawn spawner exe args =
  case spawner of
    Plain -> spawnProcess exe args
    DyingWithParent {self, parent, requests} -> do
      result <- newEmptyMVar
      putMVar requests do
        try @SomeException (spawnProcess self (parentDeathFlag : show parent : exe : args)) >>= putMVar result
      takeMVar result >>= either throwIO pure

-- | The first argument under which buck-proxy execs a server instead of serving.
parentDeathFlag :: String
parentDeathFlag = "--internal-exec-dying-with-parent"

-- | @PPID EXE ARGS...@: set the parent-death signal and exec @EXE@.
execDyingWithParent :: [String] -> IO ()
execDyingWithParent = \case
  ppidArg : exe : args | Just ppid <- readMaybe ppidArg -> do
    setParentDeathSignal
    -- Set after the fork, so a parent that died in between has already gone and the signal will never come.
    actual <- getParentProcessID
    if actual /= ppid
    then failWith ("parent " ++ show ppid ++ " exited before " ++ exe ++ " started")
    else executeFile exe False args Nothing
  _ -> failWith "expected PPID EXE ARGS..."
  where
    failWith msg = do
      hPutStrLn stderr ("buck-proxy " ++ parentDeathFlag ++ ": " ++ msg)
      exitImmediately (ExitFailure 1)

parentDeathSupported :: Bool
setParentDeathSignal :: IO ()

#if defined(linux_HOST_OS)
foreign import ccall unsafe "sys/prctl.h prctl"
  c_prctl :: CInt -> CULong -> CULong -> CULong -> CULong -> IO CInt

parentDeathSupported = True

setParentDeathSignal = do
  -- PR_SET_PDEATHSIG is 1 in linux/prctl.h.
  rc <- c_prctl 1 (fromIntegral sigKILL) 0 0 0
  if rc == 0 then pure () else hPutStrLn stderr "buck-proxy: prctl(PR_SET_PDEATHSIG) failed"
#else
parentDeathSupported = False

setParentDeathSignal = pure ()
#endif
