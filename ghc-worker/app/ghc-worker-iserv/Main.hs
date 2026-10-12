{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The worker's external interpreter for forked evals (P1b-5, an experiment, not a feature).
--
-- GHC 9.10.1's iserv ('GHCi.Server') with one difference: an eval the worker arms runs in a child forked from this
-- process, which holds the session's loaded image, and the child is discarded with whatever the eval left behind. This
-- process is built without @-threaded@, so it has no IO-manager threads, and forking it is the plain Unix fork. Forking
-- the worker's threaded session instead left the child without an IO manager, and its first socket-using eval hung
-- (plan.md 18p).
--
-- The worker arms an eval in band, by looking up a symbol named @ghc_worker_arm_fork <payload>@ just before it sends the
-- 'EvalStmt', so the arming is ordered with the eval on the one pipe and acknowledged by the reply. The payload carries
-- what a test binary's process would have had (working directory, arguments, environment, files for its standard
-- output and error), applied in the child only.
--
-- No reference created in the child may reach the GHC side: it would name a heap object that dies with the child, and
-- freeing it later would free whatever the parent has at that StablePtr. So the child replies only with a result that
-- carries none (an empty 'EvalSuccess', or an exception) and refuses anything else loudly.
module Main (main) where

import Control.Exception
import Control.Monad
import Data.Binary (Binary, put)
import Data.IORef
import Data.List (isPrefixOf)
import GHC.Clock (getMonotonicTime)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import GHCi.Message
import GHCi.RemoteTypes (HValueRef, RemotePtr)
import GHCi.Run (run)
import GHCi.Signals (installSignalHandlers)
import GHCi.TH (GHCiQException (..), runModFinalizerRefs, runTH)
import GHCi.Utils (readGhcHandle)
import IservTrace (armPrefix, disarmName, traceLine)
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv, setEnv, unsetEnv, withArgs)
import System.Exit
import System.IO
import System.Posix.Process (exitImmediately, forkProcess, getProcessStatus)

-- | What the worker armed: working directory, arguments, environment, and where standard output and error go.
type Armed = (FilePath, [String], [(String, String)], Maybe FilePath, Maybe FilePath)


main :: IO ()
main = do
  args <- getArgs
  (outh, inh) <- case args of
    arg0 : arg1 : _ -> (,) <$> readGhcHandle arg0 <*> readGhcHandle arg1
    _ -> die "usage: ghc-worker-iserv-dyn <write-fd> <read-fd>"
  installSignalHandlers
  leftovers <- newIORef Nothing
  armed <- newIORef Nothing
  trace <- traceHandle
  let pipe = Pipe {pipeRead = inh, pipeWrite = outh, pipeLeftovers = leftovers}
  -- No asynchronous exceptions while talking to GHC, as in GHCi.Server, or the protocol loses sync.
  uninterruptibleMask \ restore -> serve trace armed pipe restore

-- | With GHC_WORKER_ISERV_LOG set, one line per message this process receives and per fork, flushed as written, so a
-- crash leaves the last message it handled. Opened once, before any fork; a child inherits it and writes nothing.
traceHandle :: IO (String -> IO ())
traceHandle =
  lookupEnv "GHC_WORKER_ISERV_LOG" >>= \case
    Nothing -> pure (\ _ -> pure ())
    Just path -> do
      h <- openFile (path ++ ".trace") AppendMode
      hSetBuffering h LineBuffering
      pure (hPutStrLn h)

serve :: (String -> IO ()) -> IORef (Maybe Armed) -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serve trace armed pipe restore = loop
  where
    loop = do
      Msg msg <- readPipe pipe getMessage
      trace (traceLine (Msg msg))
      discardCtrlC
      case msg of
        Shutdown -> pure ()
        LookupSymbol name
          | armPrefix `isPrefixOf` name -> do
              writeIORef armed (Just (read (drop (length armPrefix) name)))
              trace "armed"
              reply (Nothing :: Maybe (RemotePtr ()))
          | name == disarmName -> writeIORef armed Nothing >> reply (Nothing :: Maybe (RemotePtr ()))
        EvalStmt opts expr -> do
          a <- readIORef armed
          writeIORef armed Nothing
          case a of
            Nothing -> run msg >>= reply
            Just ctx -> trace "fork" >> forkedEval ctx (run (EvalStmt opts expr)) >> trace "resumed" >> loop
        RunTH st q ty loc -> wrapRunTH (runTH pipe st q ty loc)
        RunModFinalizers st qrefs -> wrapRunTH (runModFinalizerRefs pipe st qrefs)
        _ -> run msg >>= reply

    reply :: forall a. (Binary a) => a -> IO ()
    reply r = writePipe pipe (put r) >> loop

    -- One armed eval in a child. The child applies the request's process state, runs the eval, checks that its result
    -- carries no reference, writes the reply to GHC itself, and exits without running finalizers. The parent waits.
    forkedEval :: Armed -> IO (EvalStatus [HValueRef]) -> IO ()
    forkedEval (cwd, args, env, out, err) eval = do
      hFlush stdout >> hFlush stderr
      t0 <- getMonotonicTime
      pid <- forkProcess do
        logh <- logHandle
        setCurrentDirectory cwd
        unless (null env) do
          getEnvironment >>= mapM_ (unsetEnv . fst)
          mapM_ (uncurry setEnv) env
        mapM_ (redirect stdout) out
        mapM_ (redirect stderr) err
        t1 <- getMonotonicTime
        f1 <- minorFaults
        s1 <- rtsStats
        status <- withArgs args eval `catch` \ (e :: SomeException) -> pure (EvalComplete 0 (EvalException (toSerializableException e)))
        hFlush stdout >> hFlush stderr
        t2 <- getMonotonicTime
        s2 <- rtsStats
        f2 <- minorFaults
        checked <- case status of
          EvalComplete _ (EvalSuccess []) -> pure status
          EvalComplete _ (EvalException _) -> pure status
          other -> do
            hPutStrLn logh ("ghc-worker-iserv: REFUSED a forked eval's result, it would carry references out of the child: " ++ show other)
            pure (EvalComplete 0 (EvalException (EOtherException "ghc-worker-iserv: a forked eval may return no references (only IO () statements)")))
        writePipe pipe (put checked)
        rollup <- readFile "/proc/self/smaps_rollup" `catch` \ (_ :: SomeException) -> pure ""
        let field k = maybe "-" (show . (`div` (1024 :: Int)) . read) (lookup k [(a, b) | a : b : _ <- words <$> lines rollup])
        hPutStrLn logh ("ghc-worker-iserv: forked eval: fork_ms " ++ ms t0 t1 ++ " eval_ms " ++ ms t1 t2
          ++ " private_dirty_mb " ++ field "Private_Dirty:" ++ " rss_mb " ++ field "Rss:" ++ " minflt " ++ maybe "-" show ((-) <$> f2 <*> f1) ++ statsDelta s1 s2 ++ " result " ++ summary checked)
        hClose logh
        exitImmediately ExitSuccess
      st <- getProcessStatus True False pid
      t3 <- getMonotonicTime
      logh <- logHandle
      rollup <- readFile "/proc/self/smaps_rollup" `catch` \ (_ :: SomeException) -> pure ""
      let parentRss = maybe "-" (show . (`div` (1024 :: Int)) . read) (lookup "Rss:" [(a, b) | a : b : _ <- words <$> lines rollup])
      hPutStrLn logh ("ghc-worker-iserv: forked eval: child " ++ show pid ++ " done " ++ ms t0 t3 ++ " ms after the fork, " ++ show st
        ++ ", parent rss_mb " ++ parentRss)
      hClose logh

    -- Where this process's own log lines go: the file GHC_WORKER_ISERV_LOG names, appended to, else a duplicate of
    -- standard error (which GHC may have pointed nowhere).
    logHandle = lookupEnv "GHC_WORKER_ISERV_LOG" >>= maybe (hDuplicate stderr) (`openFile` AppendMode)

    redirect h path = do
      f <- openFile path WriteMode
      hDuplicateTo f h
      hClose f

    summary = \case
      EvalComplete _ (EvalSuccess []) -> "success"
      EvalComplete _ (EvalException e) -> "exception " ++ show e
      _ -> "other"

    ms a b = show (round ((b - a) * 1000) :: Int)

    -- The child's own share of mutator and GC time, to tell an eval that computes or waits from one that collects (a
    -- major GC in a child traverses the whole retained image). Needs the RTS's -T, which the executable links in.
    rtsStats = getRTSStatsEnabled >>= \ on -> if on then Just <$> getRTSStats else pure Nothing

    statsDelta (Just a) (Just b) =
      " mutator_cpu_ms " ++ nsMs (mutator_cpu_ns b - mutator_cpu_ns a) ++ " mutator_elapsed_ms " ++ nsMs (mutator_elapsed_ns b - mutator_elapsed_ns a)
        ++ " gc_cpu_ms " ++ nsMs (gc_cpu_ns b - gc_cpu_ns a) ++ " gc_elapsed_ms " ++ nsMs (gc_elapsed_ns b - gc_elapsed_ns a)
        ++ " gcs " ++ show (gcs b - gcs a) ++ " major_gcs " ++ show (major_gcs b - major_gcs a)
    statsDelta _ _ = " rts_stats off"

    nsMs ns = show (ns `div` 1000000)

    -- Minor page faults so far (/proc/self/stat field 10). In a child, a first write to a page of the parent's image is
    -- a copy-on-write fault, so this counts the image pages an eval dirties, which mutator time alone cannot tell apart.
    minorFaults :: IO (Maybe Integer)
    minorFaults = do
      -- Strict: a lazy readFile would read the file only when the log line is written, after the eval.
      stat <- (readFile "/proc/self/stat" >>= \ s -> length s `seq` pure s) `catch` \ (_ :: SomeException) -> pure ""
      -- The command name, field 2, is parenthesised and may hold spaces; count fields after its closing parenthesis.
      pure case words (drop 1 (dropWhile (/= ')') stat)) of
        fields | length fields > 7 -> Just (read (fields !! 7))
        _ -> Nothing

    -- Run some TH code, as GHCi.Server does: it may talk to GHC with THMessages, then sends RunTHDone and a QResult.
    wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
    wrapRunTH io = do
      r <- try io
      writePipe pipe (putTHMessage RunTHDone)
      case r of
        Left e
          | Just (GHCiQException _ err) <- fromException e -> reply (QFail err :: QResult a)
          | otherwise -> showException e >>= \ str -> reply (QException str :: QResult a)
        Right a -> reply (QDone a)

    showException :: SomeException -> IO String
    showException e0 = try (evaluate (forceString (show e0))) >>= either showException pure

    forceString s = length s `seq` s

    discardCtrlC = do
      r <- try (restore (pure ()))
      case r of
        Left UserInterrupt -> discardCtrlC
        Left e -> throwIO e
        _ -> pure ()
