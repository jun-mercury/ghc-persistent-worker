module Internal.Evaluate (
  EvalRequest (..),
  evaluate,
) where

import Control.Exception (SomeException, bracket, displayException, finally, fromException)
import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC (Ghc, InteractiveImport (..), getSessionDynFlags, setContext, setInteractiveDynFlags)
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Runtime.Eval (execOptions, execStmt)
import GHC.Runtime.Eval.Types (ExecResult (..))
import Language.Haskell.Syntax.Module.Name (ModuleName)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnvironment, setEnv, unsetEnv, withArgs)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hPutStrLn, openFile, stderr, stdout)

-- | What a test binary's process would have had: the expression to run (its @main@), its arguments and environment,
-- and the files its standard output and error go to.
data EvalRequest =
  EvalRequest {
    expr :: String,
    args :: [String],
    env :: Map String String,
    stdoutFile :: Maybe FilePath,
    stderrFile :: Maybe FilePath
  }

-- | Run an @IO ()@ expression in the interactive context of a module compiled to bytecode in this session, and return
-- the exit code a process running it as @main@ would have had.
--
-- Arguments, environment, working directory and the standard handles belong to the whole process, so they are set
-- for the request and restored afterwards. A server that evaluates therefore takes one request at a time.
evaluate :: EvalRequest -> ModuleName -> Ghc Int32
evaluate req modName = do
  dflags <- getSessionDynFlags
  setInteractiveDynFlags dflags
  setContext [IIModule modName]
  inProcessState req do
    result <- execStmt req.expr execOptions
    case result of
      ExecComplete {execResult = Right _} -> pure 0
      ExecComplete {execResult = Left e} -> reifyGhc \ _ -> exitCodeOf e
      ExecBreak {} -> pure 1

-- | An 'ExitCode' thrown by the program is its exit code. Anything else is reported on the program's standard error,
-- as the RTS would report an uncaught exception, and counts as failure.
exitCodeOf :: SomeException -> IO Int32
exitCodeOf e =
  case fromException e of
    Just ExitSuccess -> pure 0
    Just (ExitFailure n) -> pure (fromIntegral n)
    Nothing -> do
      hPutStrLn stderr ("uncaught exception: " ++ displayException e)
      pure 1

inProcessState :: EvalRequest -> Ghc a -> Ghc a
inProcessState req prog =
  reifyGhc \ session ->
    withEnvironment req.env $
      withCwdRestored $
        withArgs req.args $
          redirect stdout req.stdoutFile $
            redirect stderr req.stderrFile $
              reflectGhc prog session

-- | Replace the process environment with the request's, if it sent one.
withEnvironment :: Map String String -> IO a -> IO a
withEnvironment new act
  | Map.null new = act
  | otherwise = bracket (swap (Map.toList new)) swap (const act)
  where
    swap entries = do
      old <- getEnvironment
      forM_ old (unsetEnv . fst)
      forM_ entries (uncurry setEnv)
      pure old

-- | The program may change directory, and a later request must not inherit that.
withCwdRestored :: IO a -> IO a
withCwdRestored act =
  bracket getCurrentDirectory setCurrentDirectory (const act)

redirect :: Handle -> Maybe FilePath -> IO a -> IO a
redirect _ Nothing act = act
redirect handle (Just path) act = do
  hFlush handle
  saved <- hDuplicate handle
  file <- openFile path WriteMode
  hDuplicateTo file handle
  act `finally` do
    hFlush handle
    hDuplicateTo saved handle
    hClose saved
    hClose file
