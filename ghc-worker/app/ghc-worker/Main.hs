module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GhcWorker.Run (parseCliArgs, runWorker)
import Control.Monad (when)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

-- | P1b isolation experiment: CAFs become revertible only if keepCAFs is set before they are entered.
foreign import ccall "setKeepCAFs" rts_setKeepCAFs :: IO ()

main :: IO ()
main = do
  lookupEnv "GHC_WORKER_EVAL_ISOLATION" >>= \ mode -> when (mode == Just "revert") rts_setKeepCAFs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  try (runWorker =<< parseCliArgs) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
