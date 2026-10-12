module Internal.Evaluate (
  EvalRequest (..),
  evaluate,
) where

import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException, throwIO)
import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC (Ghc, InteractiveImport (..), getSession, getSessionDynFlags, setContext, setInteractiveDynFlags, simpleImportDecl)
import GHC.Data.FastString (mkFastString)
import GHC.Driver.Env (HscEnv (..))
import GHC.Runtime.Interpreter (lookupSymbol)
import GHC.Runtime.Interpreter.Types (Interp (..), InterpInstance (..))
import qualified Control.Monad.Catch as MC
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
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hGetLine, hPutStrLn, openFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Posix.Process (exitImmediately, forkProcess, getProcessStatus)
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
  hsc_env <- getSession
  iservFork <- reifyGhc \ _ -> forkInIServ (hsc_interp hsc_env)
  case iservFork of
    Just interp -> do
      -- P1b-5: the eval runs in a child of the external interpreter, which applies the request's process state, so the
      -- worker's own is left alone.
      cwd <- reifyGhc \ _ -> getCurrentDirectory
      let armed = (cwd, req.args, Map.toList req.env, req.stdoutFile, req.stderrFile)
      _ <- reifyGhc \ _ -> lookupSymbol interp (mkFastString ("ghc_worker_arm_fork " ++ show armed))
      -- GHC binds `it` even for an IO () statement, and that reference may not leave the child. A pattern bind with no
      -- variables binds nothing; a statement that binds on purpose is sent as it is, and the interpreter refuses it.
      let stmt = if "<-" `isInfixOf` req.expr then req.expr else "() <- (" ++ req.expr ++ ")"
      runStmt stmt `MC.catch` \ (e :: SomeException) -> do
        _ <- reifyGhc \ _ -> lookupSymbol interp (mkFastString "ghc_worker_disarm_fork")
        reifyGhc \ _ -> throwIO e
    Nothing -> do
      forked <- reifyGhc \ _ -> forkThisEval
      if forked
        then reifyGhc \ session -> inForkedChild (reflectGhc (inProcessState req run) session)
        else inProcessState req run
  where
    run = runStmt req.expr

    runStmt stmt = do
      result <- execStmt stmt execOptions
      case result of
        ExecComplete {execResult = Right _} -> pure 0
        ExecComplete {execResult = Left e} -> reifyGhc \ _ -> exitCodeOf e
        ExecBreak {} -> pure 1

-- | Experiment (P1b-5): with @GHC_WORKER_ISERV_FORK_AFTER=n@ and an external interpreter (see
-- 'Internal.Session.withForkingIServ'), the first @n@ evals run in the interpreter's own process, which loads the image
-- and pays a session's one-off costs there, and every later one is armed to run in a child forked from it.
forkInIServ :: Maybe Interp -> IO (Maybe Interp)
forkInIServ = \case
  Just interp@Interp {interpInstance = ExternalInterp _} ->
    lookupEnv "GHC_WORKER_ISERV_FORK_AFTER" >>= \ setting ->
      case setting >>= readMaybe of
        Nothing -> pure Nothing
        Just after -> atomicModifyIORef' iservEvalCount \ n -> (n + 1, if n >= (after :: Int) then Just interp else Nothing)
  _ -> pure Nothing

iservEvalCount :: IORef Int
iservEvalCount = unsafePerformIO (newIORef 0)
{-# NOINLINE iservEvalCount #-}

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
  (readEnd, writeEnd) <- createPipe
  t0 <- getMonotonicTime
  pid <- forkProcess do
    closeFd readEnd
    t1 <- getMonotonicTime
    before <- getRTSStats
    code <- act `catch` exitCodeOf
    after <- getRTSStats
    t2 <- getMonotonicTime
    -- The eval's output files are flushed and closed by now ('redirect'), so the parent may answer as soon as it reads
    -- the code: the child's exit, which unmaps the parent's whole address space, happens off the request's path.
    out <- fdToHandle writeEnd
    hPutStrLn out (show code) >> hClose out
    rollup <- readFile "/proc/self/smaps_rollup" `catch` \ (_ :: SomeException) -> pure ""
    let field name = maybe "-" (show . (`div` (1024 :: Int)) . read) (lookup name [(k, v) | k : v : _ <- words <$> lines rollup])
        mb n = show (fromIntegral n `div` (1048576 :: Integer))
    hPutStrLn stderr ("ghc-worker: forked eval: fork_ms " ++ ms t0 t1 ++ " eval_ms " ++ ms t1 t2
      ++ " private_dirty_mb " ++ field "Private_Dirty:" ++ " anon_huge_mb " ++ field "AnonHugePages:" ++ " rss_mb " ++ field "Rss:"
      ++ " gcs " ++ show (gcs after - gcs before) ++ " major_gcs " ++ show (major_gcs after - major_gcs before)
      ++ " allocated_mb " ++ mb (allocated_bytes after - allocated_bytes before)
      ++ " copied_mb " ++ mb (copied_bytes after - copied_bytes before) ++ " exit " ++ show code)
    hFlush stderr
    exitImmediately (if code == 0 then ExitSuccess else ExitFailure (max 1 (fromIntegral code .&. 255)))
  closeFd writeEnd
  input <- fdToHandle readEnd
  reply <- readCode input `finally` hClose input
  t3 <- getMonotonicTime
  _ <- forkIO do
    status <- getProcessStatus True False pid
    t4 <- getMonotonicTime
    hPutStrLn stderr ("ghc-worker: forked eval: child " ++ show pid ++ " reaped " ++ ms t0 t4 ++ " ms after the fork, " ++ show status)
  hPutStrLn stderr ("ghc-worker: forked eval: parent answered " ++ ms t0 t3 ++ " ms after the fork, for " ++ show pid)
  pure (fromMaybe 1 reply)
  where
    ms a b = show (round ((b - a) * 1000) :: Int)

    -- The child's exit code, from the line it writes once the eval is done; none if it died before writing one.
    readCode h = (readMaybe <$> hGetLine h) `catch` \ (_ :: SomeException) -> pure Nothing

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
