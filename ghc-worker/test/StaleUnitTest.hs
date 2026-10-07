-- | Description: A worker that is not restarted between builds serves the first build's unit state to the second.
module StaleUnitTest where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (for_, toList)
import Data.IORef (readIORef)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Traversable (for)
import qualified Data.Set as Set
import GHC (getSession)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (DynFlags (..), GhcMode (..), targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Types.Avail (availNames)
import GHC.Types.Name (getOccString)
import GHC.Unit (stringToUnitId)
import GHC.Unit.Module.ModIface (mi_exports, mi_src_hash)
import Hedgehog (TestT, footnote, (===))
import Hedgehog.Internal.Property (Failure (..), Journal (..), Log (..), failWith, runTestT)
import Internal.AbiHash (showAbiHash)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Metadata (computeMetadata)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.Directory.Extra (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.OsPath.Extra (OsPath, fromOsPath, osp, (<.>), (</>))
import Test.Build (compileTarget, metadataArgs)
import Test.Cache (writeUnitCache)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), UnitKey (..))
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Log (withTestLog)
import Test.PackageDb (ModuleSpec (..))
import Test.Path (cachedUnitPath, compileTmpDir, moduleName, moduleOutputBase, unitName, unitTmpDir)
import Test.Run (transientSession, unitTest)
import Test.Target (fileTarget)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.Env (Env (..))
import Types.Target (TargetSpec (..))

-- | One build as Buck sends it to a worker it did not restart: the unit's extra GHC args, every module's source, then
-- the compiles in order.
data Build =
  Build {
    extraArgs :: [String],
    sources :: [(BuildModule, ByteString)],
    compiles :: [ModuleKey]
  }

-- | What one build step produced. Failures carry GHC's own text so the test output quotes the panic.
data Step =
  Step {
    label :: String,
    ok :: Bool,
    output :: [String]
  }

-- | The produced interface of one module.
data Iface =
  Iface {
    exports :: [String],
    abi :: String,
    srcHash :: String
  }
  deriving stock (Eq, Show)

-- | Flip to False to see the stale sequences fail on their own terms.
expectFailures :: Bool
expectFailures = True

-- | The sequence is red by design until the worker evicts or fingerprints unit state. Passes while the inner
-- assertions fail, printing their report to stderr so the stale output stays visible in a green run; fails, naming
-- itself, once the sequence comes out fresh, which is the signal to delete the wrapper.
stillStale :: String -> TestT IO () -> TestT IO ()
stillStale name inner
  | expectFailures = do
      (result, Journal logs) <- liftIO (runTestT inner)
      case result of
        Left (Failure location message _) ->
          liftIO $ hPutStrLn stderr $ unlines $
            ("still stale: " ++ name) : foldMap (\ l -> [show l]) location ++ message : [note | Footnote note <- logs]
        Right () ->
          failWith Nothing ("stillStale: " ++ name ++ " came out fresh; the worker no longer serves stale state here, delete the wrapper")
  | otherwise = inner

-- | Run one worker task with its own log, keeping the diagnostics and fatal errors so a failure can quote them.
-- The task's args replace the env's, as the server does, so only the task directory is prepared here.
runStep :: SessionEnv -> String -> OsPath -> (Env -> IO Bool) -> IO Step
runStep env label taskDir action =
  withTestLog False label \ (log, logVar) -> do
    createDirectoryIfMissing True (fromOsPath (env.tempDir </> taskDir))
    result <- try (action env.env {log})
    TestLog {diagnostics, fatal} <- readIORef logVar
    let logged = [d.rendered | d <- diagnostics] ++ fatal
    pure case result of
      Right ok -> Step {label, ok, output = logged}
      Left (e :: SomeException) -> Step {label, ok = False, output = logged ++ [displayException e]}

runBuild :: SessionEnv -> UnitKey -> Build -> IO [Step]
runBuild = runBuildWith True

-- | Like 'runBuild', with the metadata request optional. Buck serves a metadata action from its cache whenever the
-- key matches, so a server can receive a build's compile requests without one ever arriving.
runBuildWith :: Bool -> SessionEnv -> UnitKey -> Build -> IO [Step]
runBuildWith withMetadata env unit Build {extraArgs, sources, compiles} = do
  for_ sources \ (BuildModule {key}, content) ->
    fileTarget (fromOsPath env.sourceDir) (stringToUnitId (unitName unit)) ModuleSpec {name = moduleName key, content, boot = False}
  metadata <- if not withMetadata then pure [] else do
    step <- runStep env "metadata" (unitTmpDir unit) \ taskEnv ->
      fst <$> computeMetadata taskEnv {args = unitArgs {ghcOptions = unitArgs.ghcOptions ++ extraArgs}}
    -- Buck's metadata action writes the unit's plan, which every later compile action is given with --home-unit.
    _ <- writeUnitCache env genUnit
    pure [step]
  compiled <- for compiles \ key ->
    runStep env ("compile " ++ moduleName key) (compileTmpDir key) \ taskEnv -> do
      let compileEnv = taskEnv {args = env.shared.baseArgs {homeUnit = Just (env.tempDir </> cachedUnitPath unit)}}
          target = compileTarget key
      result <- withGhcMakeModule Compiled target compileEnv \ _targetSpec -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        compileModuleWithDepsInHpt compileEnv.log (TargetModule target)
      pure (isJust result)
  pure (metadata ++ compiled)
  where
    genUnit = GenUnit {key = unit, depUnits = [], modules = map fst sources}
    unitArgs = metadataArgs env genUnit

readIface :: SessionEnv -> ModuleKey -> TestT IO Iface
readIface env key =
  transientSession [] do
    hsc_env@HscEnv {hsc_dflags, hsc_NC} <- getSession
    iface <- liftIO (readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace path)
    pure Iface {
      exports = sort (concatMap (map getOccString . availNames) (mi_exports iface)),
      abi = showAbiHash hsc_env iface,
      srcHash = show (mi_src_hash iface)
    }
  where
    path = fromOsPath (env.tempDir </> moduleOutputBase key <.> [osp|dyn_hi|])

-- | One worker state gets every build in order; a fresh one gets only the last. The last build's module must come out
-- of both the same, and the fresh one is checked against the expected export list so the reference itself is sound.
-- | A compile-only build after a value-only edit must write the interface the changed source produces. A worker that
-- kept the previous build's module graph compiles the summary it stored instead, so the interface it writes describes
-- the old source. Buck uploads that interface under the new source's action key, so a wrong one outlives the server
-- that wrote it.
--
-- The ABI hash does not move for a value-only edit, which is why this compares the source hash.
staleInterface :: IO TestEnv -> ModuleKey -> TestT IO ()
staleInterface testEnv key = do
    shared <- liftIO testEnv
    kept <- liftIO (newSessionEnv shared)
    cold <- liftIO (newSessionEnv shared)
    let v1 = [(plain key, kValue 1)]
        v100 = [(plain key, kValue 100)]
    first <- liftIO (runBuild kept unit1 Build {extraArgs = [], sources = v1, compiles = [key]})
    checkSteps "first build" first
    second <- liftIO (runBuildWith False kept unit1 Build {extraArgs = [], sources = v100, compiles = [key]})
    checkSteps "compile-only build" second
    reference <- liftIO (runBuild cold unit1 Build {extraArgs = [], sources = v100, compiles = [key]})
    checkSteps "cold worker" reference
    keptIface <- readIface kept key
    coldIface <- readIface cold key
    footnote ("kept worker: " ++ show keptIface)
    footnote ("cold worker: " ++ show coldIface)
    keptIface.srcHash === coldIface.srcHash
    keptIface.abi === coldIface.abi

staleSequence :: IO TestEnv -> ModuleKey -> [String] -> NonEmpty Build -> TestT IO ()
staleSequence testEnv key expectedExports builds = do
    shared <- liftIO testEnv
    long <- liftIO (newSessionEnv shared)
    fresh <- liftIO (newSessionEnv shared)
    longSteps <- liftIO (concat <$> traverse (runBuild long unit1) builds)
    freshSteps <- liftIO (runBuild fresh unit1 (NonEmpty.last builds))
    checkSteps "long-lived worker" longSteps
    checkSteps "fresh worker" freshSteps
    longIface <- readIface long key
    freshIface <- readIface fresh key
    footnote ("long-lived worker: " ++ show longIface)
    footnote ("fresh worker: " ++ show freshIface)
    freshIface.exports === expectedExports
    longIface.exports === freshIface.exports
    longIface.abi === freshIface.abi

checkSteps :: String -> [Step] -> TestT IO ()
checkSteps worker steps =
  for_ (nonEmpty [s | s <- steps, not s.ok]) \ failed ->
    failWith Nothing $ intercalate "\n" $ concat [(worker ++ ": " ++ s.label ++ " failed") : s.output | s <- toList failed]

-- | A sequence the worker still serves stale. The wrapper comes off in the patch that makes it fresh, which is what
-- its failure message asks for.
stillStaleSequence :: IO TestEnv -> String -> ModuleKey -> [String] -> NonEmpty Build -> TestT IO ()
stillStaleSequence testEnv name key expectedExports builds =
  stillStale name (staleSequence testEnv key expectedExports builds)

-- | The same comparison as 'staleSequence', except that every build after the first reaches the long-lived worker as
-- compile requests alone. Buck serves a metadata action from its cache when the key matches, so a server can see a
-- later commit's compiles without any metadata request arriving.
compileOnlySequence :: IO TestEnv -> ModuleKey -> [String] -> NonEmpty Build -> TestT IO ()
compileOnlySequence testEnv key expectedExports builds =
  compileOnlySequenceRef testEnv key expectedExports (NonEmpty.last builds) builds

-- | 'compileOnlySequence' with the cold worker's build given separately. The
-- long-lived worker's last build may leave a module out, to stand for one buck2
-- does not recompile, and a cold worker has to build that module to serve the
-- same request at all.
compileOnlySequenceRef :: IO TestEnv -> ModuleKey -> [String] -> Build -> NonEmpty Build -> TestT IO ()
compileOnlySequenceRef testEnv key expectedExports reference builds = do
    shared <- liftIO testEnv
    long <- liftIO (newSessionEnv shared)
    fresh <- liftIO (newSessionEnv shared)
    firstSteps <- liftIO (runBuild long unit1 (NonEmpty.head builds))
    laterSteps <- liftIO (traverse (runBuildWith False long unit1) (NonEmpty.tail builds))
    freshSteps <- liftIO (runBuild fresh unit1 reference)
    checkSteps "long-lived worker" (concat (firstSteps : laterSteps))
    checkSteps "fresh worker" freshSteps
    longIface <- readIface long key
    freshIface <- readIface fresh key
    footnote ("long-lived worker: " ++ show longIface)
    footnote ("fresh worker: " ++ show freshIface)
    freshIface.exports === expectedExports
    longIface.exports === freshIface.exports

unit1 :: UnitKey
unit1 = UnitKey 1

k, m, k2 :: ModuleKey
k = ModuleKey {unit = unit1, number = 1, errorVariant = Nothing}
m = ModuleKey {unit = unit1, number = 2, errorVariant = Nothing}
k2 = ModuleKey {unit = unit1, number = 3, errorVariant = Nothing}

plain :: ModuleKey -> BuildModule
plain key = BuildModule {key, deps = [], th = False, bindings = 1, extDeps = []}

source :: [ByteString] -> ByteString
source = ByteString.unlines

kValue :: Int -> ByteString
kValue n = source ["module Unit1Module1 where", "value_1_1 :: Int", "value_1_1 = " <> ByteString.pack (show n)]

kValueAndExtra :: ByteString
kValueAndExtra = kValue 1 <> source ["value_1_1_1 :: Int", "value_1_1_1 = 100"]

kCpp :: ByteString
kCpp = source [
  "{-# LANGUAGE CPP #-}",
  "module Unit1Module1 where",
  "value_1_1 :: Int",
  "value_1_1 = 1",
  "#ifdef FOO",
  "value_1_1_foo :: Int",
  "value_1_1_foo = 42",
  "#endif"
  ]

-- | The splice puts K's value into a declaration name, because a value-only change does not move the ABI hash.
mSplice :: ByteString
mSplice = source [
  "{-# LANGUAGE TemplateHaskell #-}",
  "module Unit1Module2 where",
  "import Language.Haskell.TH (mkName, sigD, valD, varP, normalB, conT)",
  "import Unit1Module1 (value_1_1)",
  "$(let n = mkName (\"spliced_\" ++ show value_1_1) in sequence [sigD n (conT ''Int), valD (varP n) (normalB [| value_1_1 |]) []])"
  ]

mImportsK :: ByteString
mImportsK = source ["module Unit1Module2 where", "import Unit1Module1", "value_1_2 :: Int", "value_1_2 = value_1_1 + 1"]

-- | N relays K's value, and M's splice reaches K only through it. N is not
-- recompiled in the second build, so its bytecode still holds the old value
-- unless the loader drops every loaded importer of the edited module too.
nRelaysK :: ByteString
nRelaysK = source [
  "module Unit1Module3 where",
  "import Unit1Module1 (value_1_1)",
  "relay_1_3 :: Int",
  "relay_1_3 = value_1_1"
  ]

mSpliceViaN :: ByteString
mSpliceViaN = source [
  "{-# LANGUAGE TemplateHaskell #-}",
  "module Unit1Module2 where",
  "import Language.Haskell.TH (mkName, sigD, valD, varP, normalB, conT)",
  "import Unit1Module3 (relay_1_3)",
  "$(let n = mkName (\"spliced_\" ++ show relay_1_3) in sequence [sigD n (conT \'\'Int), valD (varP n) (normalB [| relay_1_3 |]) []])"
  ]

k2Source :: ByteString
k2Source = source ["module Unit1Module3 where", "value_1_3 :: Int", "value_1_3 = 5"]

-- | The importer also gains an export that needs the new module, because a recompile from its old source text would
-- otherwise produce the same export list and, the change being value-only, the same ABI hash.
mImportsKAndK2 :: ByteString
mImportsKAndK2 = source [
  "module Unit1Module2 where",
  "import Unit1Module1",
  "import Unit1Module3",
  "value_1_2 :: Int",
  "value_1_2 = value_1_1 + value_1_3",
  "value_1_2_3 :: Int",
  "value_1_2_3 = value_1_3"
  ]

test_staleUnit :: TestTree
test_staleUnit =
  withTestEnv \ testEnv ->
    testGroup "stale unit state across builds" [
      unitTest "a compile-only build after an edit writes the new source's interface" (staleInterface testEnv k),
      unitTest "compiles only, no metadata: the second build's splice sees the new value" $
        compileOnlySequence testEnv m ["spliced_100"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), ((plain m) {th = True}, mSplice)], compiles = [k, m]},
          Build {extraArgs = [], sources = [(plain k, kValue 100), ((plain m) {th = True}, mSplice)], compiles = [k, m]}
        ],
      unitTest "compiles only: a splice reaching the edit through an unrecompiled importer sees the new value" $
        compileOnlySequenceRef testEnv m ["spliced_100"]
          Build {extraArgs = [], sources = [(plain k, kValue 100), (plain k2, nRelaysK), ((plain m) {th = True}, mSpliceViaN)], compiles = [k, k2, m]} [
          Build {extraArgs = [], sources = [(plain k, kValue 1), (plain k2, nRelaysK), ((plain m) {th = True}, mSpliceViaN)], compiles = [k, k2, m]},
          Build {extraArgs = [], sources = [(plain k, kValue 100), (plain k2, nRelaysK), ((plain m) {th = True}, mSpliceViaN)], compiles = [k, m]}
        ],
      unitTest "compiles only: the same chain with the importer recompiled sees the new value" $
        compileOnlySequence testEnv m ["spliced_100"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), (plain k2, nRelaysK), ((plain m) {th = True}, mSpliceViaN)], compiles = [k, k2, m]},
          Build {extraArgs = [], sources = [(plain k, kValue 100), (plain k2, nRelaysK), ((plain m) {th = True}, mSpliceViaN)], compiles = [k, k2, m]}
        ],
      unitTest "source changes, same args: the second build exports the new binding" $
        staleSequence testEnv k ["value_1_1", "value_1_1_1"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1)], compiles = [k]},
          Build {extraArgs = [], sources = [(plain k, kValueAndExtra)], compiles = [k]}
        ],
      unitTest "unit args change: -DFOO added, the second build exports the CPP-gated binding" $
        stillStaleSequence testEnv "unit args change" k ["value_1_1", "value_1_1_foo"] [
          Build {extraArgs = [], sources = [(plain k, kCpp)], compiles = [k]},
          Build {extraArgs = ["-DFOO"], sources = [(plain k, kCpp)], compiles = [k]}
        ],
      unitTest "TH splice reads a changed module: the second build's splice sees the new value" $
        staleSequence testEnv m ["spliced_100"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), ((plain m) {th = True}, mSplice)], compiles = [k, m]},
          Build {extraArgs = [], sources = [(plain k, kValue 100), ((plain m) {th = True}, mSplice)], compiles = [k, m]}
        ],
      unitTest "module added to a known unit: the importer's second build sees the new module's binding" $
        staleSequence testEnv m ["value_1_2", "value_1_2_3"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), (plain m, mImportsK)], compiles = [k, m]},
          Build {
            extraArgs = [],
            sources = [(plain k, kValue 1), (plain k2, k2Source), (plain m, mImportsKAndK2)],
            compiles = [k, k2, m]
          }
        ]
    ]
