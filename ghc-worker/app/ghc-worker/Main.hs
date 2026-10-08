module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GhcWorker.BazelWorker (isBazelWorkerInvocation, runBazelWorker)
import GhcWorker.Run (bazelWorkerFeatures, parseCliArgs, runWorker)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let serve
        | isBazelWorkerInvocation args = let (prelude, rest) = requestPrelude args in runBazelWorker prelude =<< bazelWorkerFeatures rest
        | otherwise = runWorker =<< parseCliArgs
  try serve >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)

-- | The Bazel-protocol worker's @--request-prelude PATH@, taken out of its
-- arguments before the feature flags are parsed; see "GhcWorker.BazelWorker".
requestPrelude :: [String] -> (Maybe FilePath, [String])
requestPrelude = \case
  "--request-prelude" : path : rest -> (Just path, rest)
  arg : rest -> fmap (arg :) (requestPrelude rest)
  [] -> (Nothing, [])
