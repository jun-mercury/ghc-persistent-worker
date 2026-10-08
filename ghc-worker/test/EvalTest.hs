-- | Description: An eval request (a compile request with --eval-main) runs the module's main from bytecode, reports its
-- exit code, and leaves what it restored and linked in the kept state, so a second eval of the same closure is warm
-- without ever running an earlier build's code.
module EvalTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe, isJust)
import GHC.Unit (stringToUnitId)
import GHC.Unit.Home.Graph (unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (homeModInfoByteCode, homeModInfoObject)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Home.Graph (HomeUnitEnv (..))
import GHC.Unit.Module (moduleName)
import Hedgehog (TestT, footnote, (===))
import Hedgehog.Internal.Property (failWith)
import qualified Internal.Evaluate as Eval
import Internal.Session (withGhcEvalModule)
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import Prelude hiding (log)
import StaleUnitTest (Build (..), Step (..), kValue, plain, runBuild, runBuildWith, runStep, unit1)
import qualified Data.ByteString.Char8 as Char8
import System.Directory.Extra (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.OsPath.Extra (fromOsPath, osp, (</>))
import Test.Build (compileTarget)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (ModuleKey (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Path (compileTmpDir, unitName)
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

data EvalResult =
  EvalResult {
    step :: Step,
    code :: Int32,
    output :: String
  }

-- | One eval request for M, as a test action would send it, with its output in a file of its own.
runEval :: SessionEnv -> String -> IO EvalResult
runEval env label = do
  let out = fromOsPath (env.tempDir </> compileTmpDir m </> [osp|eval|]) ++ "-" ++ label ++ ".stdout"
      request = Eval.EvalRequest {
        Eval.expr = "main",
        Eval.args = ["first", "second"],
        Eval.env = [("EVAL_TEST", label), ("PATH", "/nonexistent")],
        Eval.stdoutFile = Just out,
        Eval.stderrFile = Nothing
      }
  step <- runStep env ("eval " ++ label) (compileTmpDir m) \ taskEnv -> do
    let evalEnv = taskEnv {args = env.shared.baseArgs}
        target = compileTarget m
    result <- withGhcEvalModule target evalEnv \ _ ->
      Just <$> Eval.evaluate request (moduleName target.mod)
    writeFile (out ++ ".code") (show (fromMaybe (-1) result))
    pure (isJust result)
  code <- read . Char8.unpack <$> Char8.readFile (out ++ ".code")
  output <- Char8.unpack <$> Char8.readFile out
  pure EvalResult {step, code, output}

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

keptInterps :: SessionEnv -> IO Int
keptInterps env = do
  state <- readMVar env.env.state
  pure (length state.make.interps.interps)

-- | Test 2: two builds through one kept worker state with a value edit to K between them; an eval after each. An eval
-- that ran bytecode an earlier request linked for a module of the same name would print 1 and exit 0. The request's
-- environment and directory stay its own, and an eval never replaces a compiled module's object linkable with
-- bytecode alone.
evalAcrossBuilds :: IO TestEnv -> TestT IO ()
evalAcrossBuilds testEnv = do
  shared <- liftIO testEnv
  kept <- liftIO (newSessionEnv shared)
  cwdBefore <- liftIO getCurrentDirectory
  first <- liftIO (runBuild kept unit1 Build {extraArgs = [], sources = [(plain k, kValue 1), (plain m, mMain)], compiles = [k, m]})
  checkSteps "first build" first
  before <- liftIO (keptCode kept "Unit1Module1")
  r1 <- liftIO (runEval kept "one")
  checkSteps "first eval" [r1.step]
  after <- liftIO (keptCode kept "Unit1Module1")
  footnote ("K before and after the eval (bytecode, object): " ++ show (before, after))
  -- What the compile left with an object linkable keeps it.
  fmap snd before === fmap snd after
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
  r1 <- liftIO (runEval kept "inline-one")
  checkSteps "first eval" [r1.step]
  second <- liftIO (runBuildWith False kept unit1 Build {extraArgs = [], sources = [(plain k, kInline 2), (plain m, mInline)], compiles = [k, m]})
  checkSteps "second build" second
  r2 <- liftIO (runEval kept "inline-two")
  checkSteps "second eval" [r2.step]
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
        unitTest "eval after an edit to an inlined dependency runs the new value" (evalInlinedDependency testEnv)
    ]
