module Internal.Evaluate (
  EvalRequest (..),
  evaluate,
) where

import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException)
import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC (Ghc, InteractiveImport (..), getSessionDynFlags, setContext, setInteractiveDynFlags, simpleImportDecl)
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Runtime.Eval (execOptions, execStmt)
import GHC.Runtime.Eval.Types (ExecResult (..))
import Language.Haskell.Syntax.Module.Name (ModuleName)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnvironment, setEnv, unsetEnv, withArgs)
import System.Exit (ExitCode (..))
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hPutStrLn, openFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)
import System.Posix.Process (ProcessStatus (..), exitImmediately, forkProcess, getProcessStatus)
import Text.Read (readMaybe)

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
  -- An import, not the module's own top-level scope ('IIModule'): that needs the module's top-level environment, which
  -- an entry the server kept from the module's compile does not carry, while an import reads only the exports.
  setContext [IIDecl (simpleImportDecl modName)]
  forked <- reifyGhc \ _ -> forkThisEval
  if forked
    then reifyGhc \ session -> inForkedChild (reflectGhc (inProcessState req run) session)
    else inProcessState req run
  where
    run = do
      result <- execStmt req.expr execOptions
      case result of
        ExecComplete {execResult = Right _} -> pure 0
        ExecComplete {execResult = Left e} -> reifyGhc \ _ -> exitCodeOf e
        ExecBreak {} -> pure 1

-- | Experiment (P1b-4, the zygote's fork arm, not a feature): with @GHC_WORKER_EVAL_FORK_AFTER=n@, a server runs its
-- first @n@ evals in its own process, which loads the image and pays a session's one-off costs there, and every later
-- eval in a child forked from it, which is discarded with whatever the eval left behind. Unset, nothing forks.
forkThisEval :: IO Bool
forkThisEval =
  lookupEnv "GHC_WORKER_EVAL_FORK_AFTER" >>= \ setting ->
    case setting >>= readMaybe of
      Nothing -> pure False
      Just after -> atomicModifyIORef' evalCount \ n -> (n + 1, n >= (after :: Int))

evalCount :: IORef Int
evalCount = unsafePerformIO (newIORef 0)
{-# NOINLINE evalCount #-}

firstFork :: IORef Bool
firstFork = unsafePerformIO (newIORef True)
{-# NOINLINE firstFork #-}

-- | Run an eval in a child and return its exit code. The parent collects once before its first fork, so every child
-- starts from a compacted heap and the parent does not collect while children share its pages. The child reports
-- what it cost on the server's standard error: how long the fork took to reach it, how long the eval ran, and how
-- much of the parent's memory it dirtied (smaps_rollup), the number that bounds forks per node.
inForkedChild :: IO Int32 -> IO Int32
inForkedChild act = do
  first <- atomicModifyIORef' firstFork \ b -> (False, b)
  when first performMajorGC
  t0 <- getMonotonicTime
  pid <- forkProcess do
    t1 <- getMonotonicTime
    code <- act `catch` exitCodeOf
    t2 <- getMonotonicTime
    rollup <- readFile "/proc/self/smaps_rollup" `catch` \ (_ :: SomeException) -> pure ""
    let field name = maybe "-" (show . (`div` (1024 :: Int)) . read) (lookup name [(k, v) | k : v : _ <- words <$> lines rollup])
    hPutStrLn stderr ("ghc-worker: forked eval: fork_ms " ++ ms t0 t1 ++ " eval_ms " ++ ms t1 t2 ++ " private_dirty_mb " ++ field "Private_Dirty:" ++ " anon_huge_mb " ++ field "AnonHugePages:" ++ " rss_mb " ++ field "Rss:" ++ " exit " ++ show code)
    hFlush stderr
    exitImmediately (if code == 0 then ExitSuccess else ExitFailure (max 1 (fromIntegral code .&. 255)))
  status <- getProcessStatus True False pid
  t3 <- getMonotonicTime
  hPutStrLn stderr ("ghc-worker: forked eval: parent waited " ++ ms t0 t3 ++ " ms for " ++ show pid)
  pure case status of
    Just (Exited ExitSuccess) -> 0
    Just (Exited (ExitFailure n)) -> fromIntegral n
    _ -> 1
  where
    ms a b = show (round ((b - a) * 1000) :: Int)

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
