-- | Description: An eval request (a compile request with --eval-main) runs the module's main from bytecode, reports its
-- exit code, and leaves what it restored and linked in the kept state, so a second eval of the same closure is warm
-- without ever running an earlier build's code.
module EvalTest where

import Control.Concurrent.MVar (readMVar)
import Data.IORef (readIORef)
import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List (intercalate)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, isJust)
import GHC.Unit (stringToUnitId)
import GHC.Unit.Home.Graph (unitEnv_lookup_maybe)
import GHC.Fingerprint (Fingerprint)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), homeModInfoByteCode, homeModInfoObject)
import Types.Target (TargetSpec (..))
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (Compiled))
import GHC.Unit.Module.ModIface (mi_extra_decls, mi_final_exts, mi_iface_hash, mi_mod_hash)
import GHC.Utils.Outputable (showPprUnsafe)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Home.Graph (HomeUnitEnv (..))
import GHC.Unit.Module (moduleName)
import Hedgehog (TestT, assert, footnote, (===))
import Hedgehog.Internal.Property (failWith)
import qualified Internal.Evaluate as Eval
import Internal.Session (withGhcEvalModule)
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import Prelude hiding (log)
import StaleUnitTest (Build (..), Step (..), kValue, plain, runBuild, runBuildWith, runStep, unit1)
import Data.Traversable (for)
import GHC (getSession)
import Internal.AbiHash (showAbiHash)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Metadata (computeMetadata)
import Internal.Session (withGhcMakeModule)
import GHC.Driver.Session (DynFlags (..), GhcMode (..))
import Test.Build (metadataArgs)
import Test.Data.Project (GenUnit (..))
import Test.Path (moduleOutputBase)
import System.OsPath.Extra ((<.>))
import qualified Data.ByteString.Char8 as Char8
import System.Directory.Extra (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.OsPath.Extra (OsPath, fromOsPath, osp, (</>))
import Test.Build (compileTarget)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (ModuleKey (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Path (compileTmpDir, unitName)
import qualified Data.List as List
import Control.Exception (SomeException, try)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Log (withTestLog)
import Test.Path (unitTmpDir)
import Test.Run (unitTest)
import Test.Tasty (DependencyType (AllFinish), TestTree, after, testGroup)
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (InterpPool (..), MakeState (..))
import Types.Target (ModuleTarget (..))

k, m :: ModuleKey
k = ModuleKey {unit = unit1, number = 1, errorVariant = Nothing}
m = ModuleKey {unit = unit1, number = 2, errorVariant = Nothing}

-- | Prints K's value, its arguments and one variable of its environment, and fails with 3 when K's value is 100, so
-- that the exit code tells the two builds apart as well as the output does.
mMain :: ByteString
mMain = ByteString.unlines [
  "module Unit1Module2 where",
  "import Unit1Module1 (value_1_1)",
  "import System.Environment (getArgs, lookupEnv)",
  "import System.Exit (ExitCode (..), exitWith)",
  "main :: IO ()",
  "main = do",
  "  args <- getArgs",
  "  var <- lookupEnv \"EVAL_TEST\"",
  "  putStrLn (unwords [show value_1_1, unwords args, show var])",
  "  if value_1_1 == 100 then exitWith (ExitFailure 3) else pure ()"
  ]

-- | K's value behind an INLINE pragma, so that an importer compiled with -O carries the value in its own Core.
kInline :: Int -> ByteString
kInline n = ByteString.unlines [
  "module Unit1Module1 where",
  "{-# INLINE value_1_1 #-}",
  "value_1_1 :: Int",
  "value_1_1 = " <> ByteString.pack (show n)
  ]

-- | M at -O: its main inlines K's value, so its bytecode refers to no code of K's and linking main never links K.
-- An edit to K's value moves K's ABI hash and so K's code version, recompiles M with new Core, and leaves M's source
-- hash, ABI hash and flag hash where they were. That is the hole 3.2 closes.
mInline :: ByteString
mInline = ByteString.unlines [
  "{-# OPTIONS_GHC -O #-}",
  "module Unit1Module2 where",
  "import Unit1Module1 (value_1_1)",
  "main :: IO ()",
  "main = print value_1_1"
  ]

-- | 'mInline' with main kept out of M's interface: NOINLINE leaves main without an unfolding, so the value inlined into
-- its body moves neither M's source hash nor its ABI hash, only the Core the bytecode is made from.
mInlineOpaque :: ByteString
mInlineOpaque = ByteString.unlines [
  "{-# OPTIONS_GHC -O #-}",
  "module Unit1Module2 where",
  "import Unit1Module1 (value_1_1)",
  "main :: IO ()",
  "main = print value_1_1",
  "{-# NOINLINE main #-}"
  ]

data EvalResult =
  EvalResult {
    step :: Step,
    code :: Int32,
    output :: String
  }

-- | One eval request for M, as a test action would send it, with its output in a file of its own.
runEval :: SessionEnv -> String -> IO EvalResult
runEval = runEvalWith False

-- | 'runEval', printing the request's whole worker log to stderr when asked, so a passing test still shows which path
-- restored each module (a reload logs its reason, a reuse logs nothing).
runEvalWith :: Bool -> SessionEnv -> String -> IO EvalResult
runEvalWith dump env label = do
  let out = fromOsPath (env.tempDir </> compileTmpDir m </> [osp|eval|]) ++ "-" ++ label ++ ".stdout"
      request = Eval.EvalRequest {
        Eval.expr = "main",
        Eval.args = ["first", "second"],
        Eval.env = [("EVAL_TEST", label), ("PATH", "/nonexistent")],
        Eval.stdoutFile = Just out,
        Eval.stderrFile = Nothing
      }
  step <- (if dump then runStepDumped else runStep) env ("eval " ++ label) (compileTmpDir m) \ taskEnv -> do
    let evalEnv = taskEnv {args = env.shared.baseArgs}
        target = compileTarget m
    result <- withGhcEvalModule target evalEnv \ _ ->
      Just <$> Eval.evaluate request (moduleName target.mod)
    writeFile (out ++ ".code") (show (fromMaybe (-1) result))
    pure (isJust result)
  code <- read . Char8.unpack <$> Char8.readFile (out ++ ".code")
  output <- Char8.unpack <$> Char8.readFile out
  pure EvalResult {step, code, output}

-- | 'runBuild' as buck2 sends it: each compile also writes its interface's ABI hash beside it (@--abi-out@, which
-- the request handler writes and these direct compiles otherwise skip). Without the sidecar the kept-HMI check fails
-- closed ("no .hash beside the interface") and reloads every module, so a test of the reuse path never reaches it.
runBuildAsBuck :: SessionEnv -> Build -> IO [Step]
runBuildAsBuck env Build {extraArgs, sources, compiles} = do
  _ <- runBuildWith False env unit1 Build {extraArgs, sources, compiles = []}
  metadata <- runStep env "metadata" (unitTmpDir unit1) \ taskEnv ->
    fst <$> computeMetadata taskEnv {args = unitArgs {ghcOptions = unitArgs.ghcOptions ++ extraArgs}}
  compiled <- for compiles \ key ->
    runStep env ("compile " ++ show key.number) (compileTmpDir key) \ taskEnv -> do
      let compileEnv = taskEnv {args = env.shared.baseArgs}
          target = compileTarget key
          sidecar = fromOsPath (env.tempDir </> moduleOutputBase key <.> [osp|dyn_hi|]) ++ ".hash"
      result <- withGhcMakeModule Compiled target compileEnv \ _targetSpec -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        compileModuleWithDepsInHpt compileEnv.log (TargetModule target) >>= traverse \ iface -> do
          hsc_env <- getSession
          liftIO (writeFile sidecar (showAbiHash hsc_env iface))
      pure (isJust result)
  pure (metadata : compiled)
  where
    unitArgs = metadataArgs env GenUnit {key = unit1, depUnits = [], modules = map fst sources}

-- | What buck2-haskell compiles every module with: the interface also carries the Core its bytecode is made from
-- (@mi_extra_decls@), which is where an eval gets bytecode for a module it did not compile.
buckFlags :: [String]
buckFlags = ["-fbyte-code-and-object-code"]

-- | 'runStep' that also dumps every message the worker logged, info included.
runStepDumped :: SessionEnv -> String -> OsPath -> (Env -> IO Bool) -> IO Step
runStepDumped env label taskDir action =
  withTestLog True label \ (log, logVar) -> do
    createDirectoryIfMissing True (fromOsPath (env.tempDir </> taskDir))
    result <- try (action env.env {log})
    TestLog {diagnostics, fatal} <- readIORef logVar
    let logged = [d.rendered | d <- diagnostics] ++ fatal
    pure case result of
      Right ok -> Step {label, ok, output = logged}
      Left (e :: SomeException) -> Step {label, ok = False, output = logged ++ [displayException e]}

-- | The dependency graph a server's metadata step wrote for unit 1, printed so the run shows what the plan declares,
-- and whether it names K as a dependency of M.
printPlan :: String -> SessionEnv -> IO String
printPlan who env = do
  let path = fromOsPath (env.tempDir </> unitTmpDir unit1 </> [osp|dep.json|])
  plan <- readFile path
  hPutStrLn stderr ("EVALTEST plan (" ++ who ++ ") " ++ path ++ ": " ++ plan)
  pure plan

checkSteps :: String -> [Step] -> TestT IO ()
checkSteps worker steps =
  case nonEmpty [s | s <- steps, not s.ok] of
    Nothing -> pure ()
    Just failed ->
      failWith Nothing $ intercalate "\n" $ concat [(worker ++ ": " ++ s.label ++ " failed") : s.output | s <- toList failed]

-- | Whether the kept state holds a module of unit 1 with bytecode, and with an object linkable.
keptCode :: SessionEnv -> String -> IO (Maybe (Bool, Bool))
keptCode env name = do
  state <- readMVar env.env.state
  case unitEnv_lookup_maybe (stringToUnitId (unitName unit1)) state.make.hug of
    Nothing -> pure Nothing
    Just hue ->
      lookupHpt hue.homeUnitEnv_hpt (mkModuleName name) >>= \case
        Nothing -> pure Nothing
        Just hmi -> pure (Just (isJust (homeModInfoByteCode hmi), isJust (homeModInfoObject hmi)))

-- | A kept module's ABI hash and interface hash.
keptHashes :: SessionEnv -> String -> IO (Maybe (Fingerprint, Fingerprint))
keptHashes env name = do
  state <- readMVar env.env.state
  case unitEnv_lookup_maybe (stringToUnitId (unitName unit1)) state.make.hug of
    Nothing -> pure Nothing
    Just hue ->
      lookupHpt hue.homeUnitEnv_hpt (mkModuleName name) <&> fmap \ hmi ->
        (mi_mod_hash (mi_final_exts hmi.hm_iface), mi_iface_hash (mi_final_exts hmi.hm_iface))

-- | The Core a kept module's interface carries for bytecode (its extra decls), rendered.
keptCore :: SessionEnv -> String -> IO String
keptCore env name = do
  state <- readMVar env.env.state
  case unitEnv_lookup_maybe (stringToUnitId (unitName unit1)) state.make.hug of
    Nothing -> pure "no unit"
    Just hue ->
      lookupHpt hue.homeUnitEnv_hpt (mkModuleName name) <&> \case
        Nothing -> "no module"
        Just hmi -> maybe "no extra decls" showPprUnsafe (mi_extra_decls hmi.hm_iface)

keptInterps :: SessionEnv -> IO Int
keptInterps env = do
  state <- readMVar env.env.state
  pure (length state.make.interps.interps)

-- | Test 2: two builds through one kept worker state with a value edit to K between them; an eval after each. An eval
-- that ran bytecode an earlier request linked for a module of the same name would print 1 and exit 0. The request's
-- environment and directory stay its own.
evalAcrossBuilds :: IO TestEnv -> TestT IO ()
evalAcrossBuilds testEnv = do
  shared <- liftIO testEnv
  kept <- liftIO (newSessionEnv shared)
  cwdBefore <- liftIO getCurrentDirectory
  first <- liftIO (runBuild kept unit1 Build {extraArgs = [], sources = [(plain k, kValue 1), (plain m, mMain)], compiles = [k, m]})
  checkSteps "first build" first
  -- Whether an eval keeps a compiled module's object linkable is not asserted here: in this harness a compile leaves
  -- no object linkable on the kept HMI at all (bytecode, object) = (True, False), so the check could only pass. It
  -- needs a build whose kept HMIs carry objects, which the box run's compile-then-eval sequence is.
  r1 <- liftIO (runEval kept "one")
  checkSteps "first eval" [r1.step]
  second <- liftIO (runBuildWith False kept unit1 Build {extraArgs = [], sources = [(plain k, kValue 100), (plain m, mMain)], compiles = [k, m]})
  checkSteps "second build" second
  r2 <- liftIO (runEval kept "two")
  checkSteps "second eval" [r2.step]
  footnote ("evals: " ++ show ([(r1.code, r1.output), (r2.code, r2.output)] :: [(Int32, String)]))
  (r1.code, lines r1.output) === (0, ["1 first second Just \"one\""])
  (r2.code, lines r2.output) === (3, ["100 first second Just \"two\""])
  leaked <- liftIO (lookupEnv "EVAL_TEST")
  leaked === Nothing
  cwdAfter <- liftIO getCurrentDirectory
  cwdAfter === cwdBefore

-- | Test 1: a second eval of the same closure on one server is warm. The first leaves M and K in the kept state with
-- bytecode, and the second joins the interpreter the first kept, linking no new one.
evalWarm :: IO TestEnv -> TestT IO ()
evalWarm testEnv = do
  shared <- liftIO testEnv
  kept <- liftIO (newSessionEnv shared)
  first <- liftIO (runBuild kept unit1 Build {extraArgs = [], sources = [(plain k, kValue 1), (plain m, mMain)], compiles = [k, m]})
  checkSteps "build" first
  r1 <- liftIO (runEval kept "warm-1")
  checkSteps "first eval" [r1.step]
  codeAfterFirst <- liftIO (traverse (keptCode kept) ["Unit1Module1", "Unit1Module2"])
  interpsAfterFirst <- liftIO (keptInterps kept)
  r2 <- liftIO (runEval kept "warm-2")
  checkSteps "second eval" [r2.step]
  interpsAfterSecond <- liftIO (keptInterps kept)
  footnote ("kept code after the first eval (bytecode, object): " ++ show codeAfterFirst)
  footnote ("kept interpreters after each eval: " ++ show (interpsAfterFirst, interpsAfterSecond))
  map (fmap fst) codeAfterFirst === [Just True, Just True]
  interpsAfterSecond === interpsAfterFirst
  (r1.code, lines r1.output) === (0, ["1 first second Just \"warm-1\""])
  (r2.code, lines r2.output) === (0, ["1 first second Just \"warm-2\""])

-- | Test 3, red first: K's INLINE value inlined into M at -O. After an edit to K, M's Core changes under an unchanged
-- source, ABI and flag hash; an eval that keys M's kept bytecode or kept interpreter on those alone runs the old value.
evalInlinedDependency :: IO TestEnv -> TestT IO ()
evalInlinedDependency testEnv = do
  shared <- liftIO testEnv
  kept <- liftIO (newSessionEnv shared)
  first <- liftIO (runBuild kept unit1 Build {extraArgs = [], sources = [(plain k, kInline 1), (plain m, mInline)], compiles = [k, m]})
  checkSteps "first build" first
  plan <- liftIO (printPlan "inline, first build" kept)
  -- The plan comes from GHC's dependency analysis of the sources, not from the fixture's declared deps (plain's are
  -- empty); M's import of K must be in it, or the test passes for want of a dependency to get wrong.
  assert ("Unit1Module1" `List.isInfixOf` plan && "Unit1Module2" `List.isInfixOf` plan)
  r1 <- liftIO (runEval kept "inline-one")
  checkSteps "first eval" [r1.step]
  second <- liftIO (runBuildWith False kept unit1 Build {extraArgs = [], sources = [(plain k, kInline 2), (plain m, mInline)], compiles = [k, m]})
  checkSteps "second build" second
  r2 <- liftIO (runEval kept "inline-two")
  checkSteps "second eval" [r2.step]
  footnote ("evals: " ++ show ([(r1.code, r1.output), (r2.code, r2.output)] :: [(Int32, String)]))
  lines r1.output === ["1"]
  lines r2.output === ["2"]

-- | Test 3b, red first. Its builds go through 'runBuildAsBuck', so each interface has the ABI sidecar buck2's
-- requests leave beside it; without it the kept server reloads M for want of a sidecar and the reuse path is never
-- reached (run 8 on wb-test-runner-10081245 logged exactly that). Its compiles still carry no @--home-unit@, so they
-- work from the graph the metadata step built in memory rather than from a cached plan, as the stock harness does.
-- Test 3b: the second build is compiled by another server, sharing the output directory as two servers
-- of one build do, and the eval goes to the server that kept M's bytecode from the first. M's interface on disk has new
-- Core under the source hash and ABI hash the kept HMI already has, which is all a kept HMI is checked against before
-- its bytecode is reused.
evalInlinedDependencyOtherServer :: IO TestEnv -> TestT IO ()
evalInlinedDependencyOtherServer testEnv = do
  shared <- liftIO testEnv
  kept <- liftIO (newSessionEnv shared)
  first <- liftIO (runBuildAsBuck kept Build {extraArgs = buckFlags, sources = [(plain k, kInline 1), (plain m, mInlineOpaque)], compiles = [k, m]})
  checkSteps "first build" first
  r1 <- liftIO (runEval kept "opaque-one")
  checkSteps "first eval" [r1.step]
  other <- liftIO (newResumeSessionEnv kept)
  second <- liftIO (runBuildAsBuck other Build {extraArgs = buckFlags, sources = [(plain k, kInline 2), (plain m, mInlineOpaque)], compiles = [k, m]})
  checkSteps "second build, other server" second
  planKept <- liftIO (printPlan "3b, kept server" kept)
  planOther <- liftIO (printPlan "3b, other server" other)
  assert (all (\ p -> "Unit1Module1" `List.isInfixOf` p && "Unit1Module2" `List.isInfixOf` p) ([planKept, planOther] :: [String]))
  keptM <- liftIO (keptHashes kept "Unit1Module2")
  otherM <- liftIO (keptHashes other "Unit1Module2")
  footnote ("M (ABI hash, interface hash) on the kept server and on the other: " ++ show (keptM, otherM))
  -- The case needs M's ABI unchanged; with it moved, the sidecar check reloads M and this tests nothing new.
  fmap fst keptM === fmap fst otherM
  codeBefore <- liftIO (keptCode kept "Unit1Module2")
  otherCore <- liftIO (keptCore other "Unit1Module2")
  liftIO (hPutStrLn stderr ("EVALTEST 3b: kept M (bytecode, object) before the second eval: " ++ show codeBefore))
  liftIO (hPutStrLn stderr ("EVALTEST 3b: M's Core in the other server's interface: " ++ otherCore))
  liftIO (hPutStrLn stderr "EVALTEST 3b: worker log of the second eval on the kept server follows")
  r2 <- liftIO (runEvalWith True kept "opaque-two")
  checkSteps "second eval, kept server" [r2.step]
  footnote ("evals: " ++ show ([(r1.code, r1.output), (r2.code, r2.output)] :: [(Int32, String)]))
  lines r1.output === ["1"]
  lines r2.output === ["2"]

test_eval :: TestTree
test_eval =
  withTestEnv \ testEnv ->
    testGroup "eval" [
      -- Each sets the process's environment and directory for the program it runs, so they run one after another.
      unitTest "eval: a second eval of one closure is warm (kept code, kept interpreter)" (evalWarm testEnv),
      after AllFinish "is warm" $
        unitTest "eval after a value edit runs the new value, and restores environment and directory" (evalAcrossBuilds testEnv),
      after AllFinish "restores environment" $
        unitTest "eval after an edit to an inlined dependency runs the new value" (evalInlinedDependency testEnv),
      after AllFinish "inlined dependency runs" $
        unitTest "eval after another server compiled an inlined dependency's edit runs the new value" (evalInlinedDependencyOtherServer testEnv)
    ]
