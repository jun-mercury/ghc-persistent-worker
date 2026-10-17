module Main where

import BuckProxy.Run (parseCliArgs, run)
import BuckProxy.Spawn (execDyingWithParent, parentDeathFlag)
import BuckProxy.Util (dbg)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad (join, void)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import System.Timeout (timeout)
import Types.Orchestration (envServerSocket)

-- | buck2 stops a worker with SIGTERM, then SIGKILL after a grace period, and signals only buck-proxy's pid, so this is
-- where the GHC servers are stopped.
--
-- The RTS runs a 'Catch' handler in a thread of its own, where 'System.Exit.exitSuccess' only throws to that thread and
-- the process goes on serving. So the handler ends the process itself. The cleanup is bounded: it waits for the worker
-- map, which a server still starting holds, and the servers die with this process regardless (see "BuckProxy.Spawn").
onSigTERM :: MVar (IO ()) -> IO ()
onSigTERM refHandler = do
  dbg "buck-proxy: SigTERM is being handled..."
  timeout 5_000_000 (join (readMVar refHandler)) >>= \case
    Just () -> pure ()
    Nothing -> dbg "buck-proxy: stopping the GHC servers took over 5 s; exiting without them"
  hFlush stdout
  hFlush stderr
  exitImmediately ExitSuccess

main :: IO ()
main =
  getArgs >>= \case
    flag : rest | flag == parentDeathFlag -> execDyingWithParent rest
    _ -> serve

serve :: IO ()
serve = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseCliArgs
  socket <- envServerSocket
  refHandler <- newMVar (pure ())
  void $ installHandler sigTERM (Catch $ onSigTERM refHandler) Nothing
  try (run socket options refHandler) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
