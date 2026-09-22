-- | Description: A worker that is not restarted between builds must not serve the first build's unit state to the
-- second. Each sequence feeds several builds to one long-lived worker and the last build alone to a fresh worker; the
-- last module must come out of both identical, which it does only when the worker revalidates the state it kept.
module StaleUnitTest where

import Control.Exception (SomeException, displayException, try)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (for_, toList)
import Data.IORef (readIORef)
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Traversable (for)
import GHC (getSession, getSessionDynFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (DynFlags (..), GhcMode (..), targetProfile)
import GHC.Fingerprint (Fingerprint)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Types.Avail (availNames)
import GHC.Types.Name (getOccString)
import GHC.Unit (stringToUnitId)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.ModDetails (emptyModDetails)
import GHC.Unit.Module.ModIface (mi_exports, mi_src_hash)
import Hedgehog (TestT, assert, footnote, (===))
import Hedgehog.Internal.Property (failWith)
import Internal.AbiHash (showAbiHash)
import Internal.Cache.Hpt (interfaceStale, readIfaceHeader)
import Internal.Cache.Metadata (addHomeUnitTo, flagsFingerprint)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Metadata (computeMetadata)
import Internal.Session (simpleSessionWithDebugLog, withGhcMakeModule)
import Internal.State (newState)
import Prelude hiding (log)
import System.Directory.Extra (createDirectoryIfMissing, listDirectory, removeFile)
import qualified System.FilePath as FP
import System.OsPath.Extra (OsPath, fromOsPath, osp, (<.>), (</>))
import Test.Build (compileTarget, metadataArgs)
import Test.Cache (writeUnitCacheWith)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), UnitKey (..))
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Log (withTestLog)
import Test.PackageDb (ModuleSpec (..))
import Test.Path (cachedUnitPath, compileTmpDir, moduleName, moduleOutputBase, unitName, unitTmpDir)
import Test.Run (transientSession, unitTest)
import Test.Target (fileTarget)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..), emptyArgs)
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.Env (Env (..))
import Types.Target (TargetSpec (..))

-- | One build as Buck sends it to a worker it did not restart: the unit's extra GHC args, every module's source, then
-- the compiles in order.
data Build =
  Build {
    extraArgs :: [(UnitKey, [String])],
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

runBuild :: SessionEnv -> Build -> IO [Step]
runBuild = runBuildWith True

-- | Like 'runBuild', with the metadata request optional. Buck serves a metadata action from its cache whenever the
-- key matches, so a server can receive a build's compile requests without one ever arriving.
runBuildWith :: Bool -> SessionEnv -> Build -> IO [Step]
runBuildWith withMetadata env Build {extraArgs, sources, compiles} = do
  for_ sources \ (BuildModule {key}, content) ->
    fileTarget (fromOsPath env.sourceDir) (stringToUnitId (unitName key.unit)) ModuleSpec {name = moduleName key, content, boot = False}
  -- Buck's metadata action writes each unit's plan, which every later compile
  -- action of that unit is given with --home-unit. Buck materialises the plan
  -- even when it serves the action from its cache, so the plan is current
  -- whether or not a metadata request reaches this server.
  for_ units \ u -> () <$ writeUnitCacheWith env (genUnit u) (argsFor u)
  metadata <- if not withMetadata then pure [] else for units \ u ->
    runStep env ("metadata " ++ unitName u) (unitTmpDir u) \ taskEnv ->
      fst <$> computeMetadata taskEnv {args = (unitArgs u) {ghcOptions = (unitArgs u).ghcOptions ++ argsFor u}}
  compiled <- for compiles \ key ->
    runStep env ("compile " ++ moduleName key) (compileTmpDir key) \ taskEnv -> do
      let compileEnv = taskEnv {args = env.shared.baseArgs {homeUnit = Just (env.tempDir </> cachedUnitPath key.unit)}}
          target = compileTarget key
      result <- withGhcMakeModule Compiled target compileEnv \ _targetSpec -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        iface <- compileModuleWithDepsInHpt compileEnv.log (TargetModule target)
        -- Buck gives every compile --abi-out, and the loader's staleness check
        -- reads the sidecar it writes. Without one here the check can only fail
        -- closed, which is not the path production takes.
        for_ iface \ i -> do
          hsc_env <- getSession
          liftIO (writeFile (abiSidecar env key) (showAbiHash hsc_env i))
        pure iface
      pure (isJust result)
  pure (metadata ++ compiled)
  where
    -- A unit depends on every lower-numbered unit in the build, which is as
    -- much structure as these sequences need.
    units = sort (nub [key.unit | (BuildModule {key}, _) <- sources])
    genUnit u = GenUnit {
      key = u,
      depUnits = Set.fromList [v | v <- units, v < u],
      modules = [gm | (gm, _) <- sources, gm.key.unit == u]
      }
    argsFor u = concat [a | (v, a) <- extraArgs, v == u]
    unitArgs u = metadataArgs env (genUnit u)

-- | Where a compile's @--abi-out@ sidecar goes, beside the interface.
abiSidecar :: SessionEnv -> ModuleKey -> FilePath
abiSidecar env key = fromOsPath (env.tempDir </> moduleOutputBase key <.> [osp|dyn_hi|]) ++ ".hash"

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
    first <- liftIO (runBuild kept Build {extraArgs = [], sources = v1, compiles = [key]})
    checkSteps "first build" first
    second <- liftIO (runBuildWith False kept Build {extraArgs = [], sources = v100, compiles = [key]})
    checkSteps "compile-only build" second
    reference <- liftIO (runBuild cold Build {extraArgs = [], sources = v100, compiles = [key]})
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
    longSteps <- liftIO (concat <$> traverse (runBuild long) builds)
    freshSteps <- liftIO (runBuild fresh (NonEmpty.last builds))
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
    firstSteps <- liftIO (runBuild long (NonEmpty.head builds))
    laterSteps <- liftIO (traverse (runBuildWith False long) (NonEmpty.tail builds))
    freshSteps <- liftIO (runBuild fresh reference)
    checkSteps "long-lived worker" (concat (firstSteps : laterSteps))
    checkSteps "fresh worker" freshSteps
    longIface <- readIface long key
    freshIface <- readIface fresh key
    footnote ("long-lived worker: " ++ show longIface)
    footnote ("fresh worker: " ++ show freshIface)
    freshIface.exports === expectedExports
    longIface.exports === freshIface.exports

-- | An eviction must outlive a compile that is already in flight. The parked
-- request restores the unit, an eviction then replaces it, and the parked
-- request stores afterwards. Its snapshot still holds the old unit, so a
-- write-back that cannot express a removal puts the evicted one back, and
-- every later request is served from it.
parkedCompileSequence :: IO TestEnv -> TestT IO ()
parkedCompileSequence testEnv = do
  shared <- liftIO testEnv
  long <- liftIO (newSessionEnv shared)
  fresh <- liftIO (newSessionEnv shared)
  firstSteps <- liftIO (runBuild long (plainCpp []))
  started <- liftIO newEmptyMVar
  release <- liftIO newEmptyMVar
  parked <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO do
    step <- runStep long "parked compile" (compileTmpDir k) \ taskEnv -> do
      let compileEnv = taskEnv {args = long.shared.baseArgs {homeUnit = Just (long.tempDir </> cachedUnitPath unit1)}}
      result <- withGhcMakeModule Compiled (compileTarget k) compileEnv \ _targetSpec -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        iface <- compileModuleWithDepsInHpt compileEnv.log (TargetModule (compileTarget k))
        liftIO (putMVar started () *> takeMVar release)
        pure iface
      pure (isJust result)
    putMVar parked step
  liftIO (takeMVar started)
  -- While that one is parked, a metadata request redefines the unit.
  evictSteps <- liftIO (runBuild long (plainCpp ["-DFOO"]))
  liftIO (putMVar release ())
  parkedStep <- liftIO (takeMVar parked)
  -- A later build, compiles alone, must not be served the resurrected unit.
  laterSteps <- liftIO (runBuildWith False long (plainCpp ["-DFOO"]))
  freshSteps <- liftIO (runBuild fresh (plainCpp ["-DFOO"]))
  checkSteps "long-lived worker" (firstSteps ++ evictSteps ++ [parkedStep] ++ laterSteps)
  checkSteps "fresh worker" freshSteps
  longIface <- readIface long k
  freshIface <- readIface fresh k
  footnote ("long-lived worker: " ++ show longIface)
  footnote ("fresh worker: " ++ show freshIface)
  freshIface.exports === ["value_1_1", "value_1_1_foo"]
  longIface.exports === freshIface.exports
  where
    plainCpp args = Build {extraArgs = [(unit1, args)], sources = [(plain k, kCpp)], compiles = [k]}

-- | Remove everything a module's compile wrote, so the next server finds its
-- outputs absent the way a buck2 action does. A shared output directory is a
-- property of this harness, not of production.
clearModuleArtifacts :: SessionEnv -> ModuleKey -> IO ()
clearModuleArtifacts env key = do
  let path = fromOsPath (env.tempDir </> moduleOutputBase key)
      dir = FP.takeDirectory path
      base = FP.takeFileName path
  entries <- listDirectory dir
  -- Match on the extension boundary: a bare prefix would also take
  -- Unit1Module10's outputs when clearing Unit1Module1's.
  for_ [e | e <- entries, (base ++ ".") `isPrefixOf` e] \ entry ->
    removeFile (dir FP.</> entry)

-- | A dependency unit rebuilt by another server. One server recompiles unit 1
-- under its new flags into the shared output directory, the way a pool spreads
-- a build, and the server that keeps unit 1 as a dependency is then asked only
-- for unit 2. That second server never sees a request naming unit 1, so
-- nothing on the path a compile request takes can notice that unit 1 moved.
crossServerDepSequence :: IO TestEnv -> ModuleKey -> ModuleKey -> [String] -> Build -> Build -> Build -> TestT IO ()
crossServerDepSequence testEnv key dependency expectedExports before afterDep afterUse = do
  shared <- liftIO testEnv
  long <- liftIO (newSessionEnv shared)
  fresh <- liftIO (newSessionEnv shared)
  firstSteps <- liftIO (runBuild long before)
  -- A different server, with no kept state, on the same sources and outputs.
  other <- liftIO (newResumeSessionEnv long)
  liftIO (clearModuleArtifacts long dependency)
  otherSteps <- liftIO (runBuildWith False other afterDep)
  laterSteps <- liftIO (runBuildWith False long afterUse)
  freshSteps <- liftIO (runBuild fresh afterDep)
  checkSteps "other server" (firstSteps ++ otherSteps)
  checkSteps "long-lived worker" laterSteps
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

-- | The importer before it imports anything, so the edit that follows adds the
-- import rather than changing one.
mStandalone :: ByteString
mStandalone = source ["module Unit1Module2 where", "value_1_2 :: Int", "value_1_2 = 2"]

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

unit2 :: UnitKey
unit2 = UnitKey 2

-- | The module of a second unit, which splices a value from the first.
p1 :: ModuleKey
p1 = ModuleKey {unit = unit2, number = 1, errorVariant = Nothing}

-- | A CPP-gated value rather than a CPP-gated export, so that the unit flag
-- changes what a dependent's splice evaluates to rather than what it can name.
kCppValue :: ByteString
kCppValue = source [
  "{-# LANGUAGE CPP #-}",
  "module Unit1Module1 where",
  "value_1_1 :: Int",
  "#ifdef FOO",
  "value_1_1 = 100",
  "#else",
  "value_1_1 = 1",
  "#endif"
  ]

pSplicesK :: ByteString
pSplicesK = source [
  "{-# LANGUAGE TemplateHaskell #-}",
  "module Unit2Module1 where",
  "import Language.Haskell.TH (mkName, sigD, valD, varP, normalB, conT)",
  "import Unit1Module1 (value_1_1)",
  "$(let n = mkName (\"spliced_\" ++ show value_1_1) in sequence [sigD n (conT \'\'Int), valD (varP n) (normalB [| value_1_1 |]) []])"
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

-- | GHC's own flag fingerprint of a unit whose only extra args are the given ones, obtained through the same
-- 'flagsFingerprint' the worker uses to decide eviction.
flagFingerprint :: [String] -> IO Fingerprint
flagFingerprint extra = do
  st <- newState
  result <- simpleSessionWithDebugLog st (emptyArgs []) {ghcOptions = ghcOptions'} do
    hsc0 <- getSession
    dflags <- getSessionDynFlags
    (hsc1, unit) <- liftIO (addHomeUnitTo hsc0 dflags)
    liftIO (flagsFingerprint hsc1 unit dflags)
  pure (fromMaybe (error "flagFingerprint: session failed") result)
  where
    ghcOptions' = ["-hide-all-packages", "-package", "base", "-this-unit-id", "ffp", "-dynamic", "-fPIC"] ++ extra

-- | Compile K once through a worker and hand its produced interface, wrapped in a 'HomeModInfo', to a test of the
-- interface staleness check. The details and linkable are unused by 'interfaceStale', so they are left empty.
withCompiledK :: IO TestEnv -> (HscEnv -> HomeModInfo -> OsPath -> IO ()) -> TestT IO ()
withCompiledK testEnv use = do
  shared <- liftIO testEnv
  env <- liftIO (newSessionEnv shared)
  steps <- liftIO (runBuild env Build {extraArgs = [], sources = [(plain k, kValue 1)], compiles = [k]})
  for_ (nonEmpty [s | s <- steps, not s.ok]) \ failed ->
    failWith Nothing (intercalate "\n" [s.label ++ " failed\n" ++ intercalate "\n" s.output | s <- toList failed])
  let path = env.tempDir </> moduleOutputBase k <.> [osp|dyn_hi|]
  transientSession [] do
    hsc_env <- getSession
    liftIO do
      iface <- readBinIface (targetProfile hsc_env.hsc_dflags) hsc_env.hsc_NC IgnoreHiWay QuietBinIFace (fromOsPath path)
      let hmi = HomeModInfo {hm_iface = iface, hm_details = emptyModDetails, hm_linkable = HomeModLinkable Nothing Nothing}
      use hsc_env hmi path

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
      unitTest "compiles only: a unit args change exports the CPP-gated binding" $
        compileOnlySequence testEnv k ["value_1_1", "value_1_1_foo"] [
          Build {extraArgs = [], sources = [(plain k, kCpp)], compiles = [k]},
          Build {extraArgs = [(unit1, ["-DFOO"])], sources = [(plain k, kCpp)], compiles = [k]}
        ],
      unitTest "compiles only: a dependency unit's args change reaches the dependent's splice" $
        compileOnlySequence testEnv p1 ["spliced_100"] [
          Build {
            extraArgs = [],
            sources = [(plain k, kCppValue), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          },
          Build {
            extraArgs = [(unit1, ["-DFOO"])],
            sources = [(plain k, kCppValue), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          }
        ],
      unitTest "an eviction outlives a compile that is already in flight" (parkedCompileSequence testEnv),


      unitTest "compiles only: a dependency unit rebuilt by another server reaches the dependent's splice" $
        crossServerDepSequence testEnv p1 k ["spliced_100"]
          Build {
            extraArgs = [],
            sources = [(plain k, kCppValue), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          }
          Build {
            extraArgs = [(unit1, ["-DFOO"])],
            sources = [(plain k, kCppValue), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          }
          Build {
            extraArgs = [(unit1, ["-DFOO"])],
            sources = [(plain k, kCppValue), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [p1]
          },
      unitTest "compiles only: a dependency unit's edited value reaches the dependent's splice across servers" $
        crossServerDepSequence testEnv p1 k ["spliced_100"]
          Build {
            extraArgs = [],
            sources = [(plain k, kValue 1), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          }
          Build {
            extraArgs = [],
            sources = [(plain k, kValue 100), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [k, p1]
          }
          Build {
            extraArgs = [],
            sources = [(plain k, kValue 100), ((plain p1) {th = True, deps = Set.fromList [k]}, pSplicesK)],
            compiles = [p1]
          },
      unitTest "compiles only: a module added to the unit reaches the importer" $
        compileOnlySequenceRef testEnv m ["value_1_2"]
          Build {extraArgs = [], sources = [(plain k, kValue 1), ((plain m) {deps = Set.fromList [k]}, mImportsK)], compiles = [k, m]} [
          Build {extraArgs = [], sources = [(plain m, mStandalone)], compiles = [m]},
          Build {extraArgs = [], sources = [(plain k, kValue 1), ((plain m) {deps = Set.fromList [k]}, mImportsK)], compiles = [k, m]}
        ],
      unitTest "source changes, same args: the second build exports the new binding" $
        staleSequence testEnv k ["value_1_1", "value_1_1_1"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1)], compiles = [k]},
          Build {extraArgs = [], sources = [(plain k, kValueAndExtra)], compiles = [k]}
        ],
      unitTest "unit args change: -DFOO added, the second build exports the CPP-gated binding" $
        staleSequence testEnv k ["value_1_1", "value_1_1_foo"] [
          Build {extraArgs = [], sources = [(plain k, kCpp)], compiles = [k]},
          Build {extraArgs = [(unit1, ["-DFOO"])], sources = [(plain k, kCpp)], compiles = [k]}
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
        ],
      unitTest "flag fingerprint ignores output dirs but tracks a preprocessor define" do
        rootA <- liftIO (flagFingerprint ["-odir", "/tmp/root-a", "-hidir", "/tmp/root-a"])
        rootB <- liftIO (flagFingerprint ["-odir", "/tmp/root-b", "-hidir", "/tmp/root-b"])
        withFoo <- liftIO (flagFingerprint ["-odir", "/tmp/root-a", "-hidir", "/tmp/root-a", "-DFOO"])
        footnote ("two roots: " ++ show rootA ++ " vs " ++ show rootB)
        rootA === rootB
        assert (rootA /= withFoo),
      unitTest "interface header source hash agrees with the full interface" $
        withCompiledK testEnv \ _ hmi path ->
          readIfaceHeader (fromOsPath path) >>= \case
            Left e -> ioError (userError e)
            Right srcHash
              | srcHash == mi_src_hash hmi.hm_iface -> pure ()
              | otherwise -> ioError (userError "header source hash disagrees with mi_src_hash"),
      unitTest "interface check fails closed on a missing sidecar and passes on a matching one" $
        withCompiledK testEnv \ hsc_env hmi path -> do
          let sidecar = fromOsPath path ++ ".hash"
          -- The harness writes the sidecar with every compile, as a client does, so the missing case is made here.
          removeFile sidecar
          missing <- interfaceStale hsc_env hmi path
          writeFile sidecar (showAbiHash hsc_env hmi.hm_iface)
          matching <- interfaceStale hsc_env hmi path
          writeFile sidecar "deadbeefdeadbeefdeadbeefdeadbeef"
          wrong <- interfaceStale hsc_env hmi path
          removeFile sidecar
          checkEq "missing sidecar" missing (Just "no .hash beside the interface")
          checkEq "matching sidecar" matching Nothing
          checkEq "wrong sidecar" wrong (Just "ABI hash changed")
    ]
  where
    checkEq what actual expected
      | actual == expected = pure ()
      | otherwise = ioError (userError (what ++ ": got " ++ show actual ++ ", expected " ++ show expected))
