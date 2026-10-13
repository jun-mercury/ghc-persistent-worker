-- | Description: One server serves the builds of two commits at once. Each scenario holds a request of commit X
-- after it restored its state, runs a whole request of commit Y, then lets X go on, which is the interleaving a shared
-- server on a remote executor sees whenever two builds overlap. X must compile what X's own root says, Y's work must
-- stay in the server for the next Y request, and neither may see the other's units, modules or linked code.
--
-- A root is an execution root the way an executor lays one out: the same relative paths in every root (@src@, @out@,
-- @plan@), different contents per commit. Every request runs in its root's working directory through
-- 'withRequestCwd', as the server runs it.
module ConcurrentTest where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import GHC (getSession, mkModule, mkModuleName)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (DynFlags (..), GhcMode (..), targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Types.Avail (availNames)
import GHC.Types.Name (getOccString)
import GHC.Unit (stringToUnit, stringToUnitId)
import GHC.Unit.Env (HomeUnitEnv (..))
import GHC.Unit.Home.Graph (unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Module.ModIface (mi_exports)
import GhcWorker.RequestCwd (newProcessCwdLock, requestCwdVar, withRequestCwd)
import Hedgehog (TestT, footnote, (===))
import Hedgehog.Internal.Property (failWith)
import Internal.AbiHash (showAbiHash)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Session (withGhcMakeModule)
import Internal.State (newState)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import System.Timeout (timeout)
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Log (withTestLog)
import Test.Run (transientSession, unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..), emptyArgs)
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.CachedDeps (
  CachedBuildPlan (..),
  CachedBuildPlans (..),
  CachedDep (..),
  CachedDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..), defaultFeatureFlags)
import Types.Grpc (CommandEnv (..))
import Types.State (Options (..), WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.Target (ModuleTarget (..), TargetSpec (..))

-- | A module of the test unit: its name, the modules of the unit it imports, and its source.
data Mod =
  Mod {
    name :: String,
    deps :: [String],
    text :: ByteString
  }

unitName :: String
unitName = "unit1"

planPath, argsPath :: FilePath
planPath = "plan" </> "cached_unit.json"
argsPath = "plan" </> "unit_args"

srcPath, hiPath :: String -> FilePath
srcPath name = "src" </> name ++ ".hs"
hiPath name = "out" </> name ++ ".dyn_hi"

-- | Lay out a root the way the rules would: sources, the unit's args file and its build plan, all paths relative to
-- the root, and the unit args that name the root-relative output directory.
writeRoot :: FilePath -> [String] -> [Mod] -> IO ()
writeRoot root extraArgs mods = do
  for_ @[] ["src", "plan", "out", "tmp"] \ d -> createDirectoryIfMissing True (root </> d)
  for_ mods \ m -> ByteString.writeFile (root </> srcPath m.name) m.text
  writeFile (root </> argsPath) (unlines (unitArgs ++ extraArgs))
  Aeson.encodeFile (root </> planPath) CachedUnit {
    build_plan = Just plan,
    cache = Nothing,
    is_binary = False,
    unit_args = Just (toOsPath argsPath),
    unit_buck_args = Nothing,
    dep_units = Nothing
  }
  where
    plan =
      Map.fromList [
        (JsonFs (mkModuleName m.name), CachedModule {
          source = toOsPath (srcPath m.name),
          modules = JsonFs . mkModuleName <$> m.deps,
          packages = [],
          flags = []
        })
        | m <- mods
      ]

    unitArgs = [
      "-i",
      "-hide-all-packages",
      "-package", "base",
      "-package", "template-haskell",
      "-this-unit-id", unitName,
      "-odir", "out",
      "-hidir", "out",
      "-dynamic",
      "-fbyte-code-and-object-code",
      "-fprefer-byte-code",
      "-fPIC",
      "-osuf", "dyn_o",
      "-hisuf", "dyn_hi"
      ]

-- | What one request produced, with GHC's own text when it failed.
data Step =
  Step {
    label :: String,
    ok :: Bool,
    output :: [String]
  }

-- | One compile request as a remote executor's client sends it: the module, the unit's plan and the module's
-- dependency closure, all relative to the root, which is the request's working directory.
compileIn :: MVar WorkerState -> FilePath -> String -> [String] -> IO Step
compileIn state root modName deps = do
  lock <- newProcessCwdLock
  withTestLog False label \ (log, logVar) -> do
    result <- try $ withRequestCwd lock (CommandEnv (Map.singleton requestCwdVar root)) False do
      let env = Env {log, state, args}
      compiled <- withGhcMakeModule Compiled target env \ _ -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        compileModuleWithDepsInHpt log (TargetModule target)
      pure (isJust compiled)
    TestLog {diagnostics, fatal} <- readIORef logVar
    let logged = [d.rendered | d <- diagnostics] ++ fatal
    pure case result of
      Right ok -> Step {label, ok, output = logged}
      Left (e :: SomeException) -> Step {label, ok = False, output = logged ++ [displayException e]}
  where
    label = modName ++ " in " ++ root

    target = ModuleTarget {mod = mkModule (stringToUnit unitName) (mkModuleName modName)}

    args =
      (emptyArgs []) {
        homeUnit = Just (toOsPath planPath),
        cachedDeps = Just (CachedDeps [CachedDep {name = JsonFs (mkModuleName d), package = JsonFs (stringToUnitId unitName)} | d <- deps]),
        tempDir = Just (toOsPath (root </> "tmp"))
      }

-- | Run request X up to the end of its restore, run all of request Y, then let X finish. The server's own hook holds
-- X, so the interleaving is the same on every run.
interleave :: MVar WorkerState -> IO Step -> IO Step -> IO (Step, Step)
interleave state requestX requestY = do
  restored <- newEmptyMVar
  resume <- newEmptyMVar
  holds <- newIORef [putMVar restored () >> readMVar resume]
  modifyMVar_ state \ s -> pure s {options = s.options {afterRestore = next holds}}
  doneX <- newEmptyMVar
  _ <- forkIO (requestX >>= putMVar doneX)
  waited <- timeout 120_000_000 (takeMVar restored)
  stepY <- case waited of
    Nothing -> pure Step {label = "request X", ok = False, output = ["request X did not reach the end of its restore in 120 s"]}
    Just () -> bounded "request Y" requestY
  putMVar resume ()
  stepX <- bounded "request X" (takeMVar doneX)
  pure (stepX, stepY)
  where
    next holds = do
      hold <- atomicModifyIORef' holds \case
        h : rest -> (rest, h)
        [] -> ([], pure ())
      hold

    -- Y runs while X is held. Where unshare(CLONE_FS) is refused, X holds the process working directory and Y waits
    -- for it forever, so the wait is bounded and says why.
    bounded what run =
      timeout 300_000_000 run >>= \case
        Just step -> pure step
        Nothing -> pure Step {label = what, ok = False, output = [what ++ " did not finish in 300 s; is unshare(CLONE_FS) refused here?"]}

checkSteps :: [Step] -> TestT IO ()
checkSteps steps =
  for_ [s | s <- steps, not s.ok] \ s ->
    failWith Nothing (intercalate "\n" ((s.label ++ " failed") : s.output))

-- | The produced interface of a module in a root.
data Iface =
  Iface {
    exports :: [String],
    abi :: String
  }
  deriving stock (Eq, Show)

readIfaceIn :: FilePath -> String -> TestT IO Iface
readIfaceIn root name =
  transientSession [] do
    hsc_env@HscEnv {hsc_dflags, hsc_NC} <- getSession
    iface <- liftIO (readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace (root </> hiPath name))
    pure Iface {exports = ifaceExports iface, abi = showAbiHash hsc_env iface}
  where
    ifaceExports iface = sort (concatMap (map getOccString . availNames) (mi_exports iface))

-- | The exports of the module the server keeps for the unit, which is what the next request of any build starts from.
keptExports :: MVar WorkerState -> String -> IO (Maybe [String])
keptExports state name = do
  s <- readMVar state
  case unitEnv_lookup_maybe (stringToUnitId unitName) s.make.hug of
    Nothing -> pure Nothing
    Just hue ->
      lookupHpt (homeUnitEnv_hpt hue) (mkModuleName name) >>= \case
        Nothing -> pure Nothing
        Just hmi -> pure (Just (sort (concatMap (map getOccString . availNames) (mi_exports hmi.hm_iface))))

k, m :: String
k = "Unit1Module1"
m = "Unit1Module2"

hs :: [ByteString] -> ByteString
hs = ByteString.unlines

kCpp :: ByteString
kCpp = hs [
  "{-# LANGUAGE CPP #-}",
  "module Unit1Module1 where",
  "value_1_1 :: Int",
  "value_1_1 = 1",
  "#ifdef FOO",
  "value_1_1_foo :: Int",
  "value_1_1_foo = 42",
  "#endif"
  ]

kTyped :: ByteString -> ByteString
kTyped ty = hs ["module Unit1Module1 where", "value_1_1 :: " <> ty, "value_1_1 = 1"]

-- | The importer's exported type is whatever its dependency's is, so the interface it produces names the dependency
-- it was compiled against.
mInfers :: ByteString
mInfers = hs ["module Unit1Module2 where", "import Unit1Module1", "value_1_2 = value_1_1"]

kValue :: Int -> ByteString
kValue n = hs ["module Unit1Module1 where", "value_1_1 :: Int", "value_1_1 = " <> ByteString.pack (show n)]

-- | The splice puts K's value into a declaration name, because a value-only change does not move the ABI hash.
mSplice :: ByteString
mSplice = hs [
  "{-# LANGUAGE TemplateHaskell #-}",
  "module Unit1Module2 where",
  "import Language.Haskell.TH (mkName, sigD, valD, varP, normalB, conT)",
  "import Unit1Module1 (value_1_1)",
  "$(let n = mkName (\"spliced_\" ++ show value_1_1) in sequence [sigD n (conT ''Int), valD (varP n) (normalB [| value_1_1 |]) []])"
  ]

-- | Commit Y adds @-DFOO@ to the unit's args, so the server evicts the unit X restored and restores Y's while X's
-- request is still running. X's write-back must not replace Y's unit: the next Y request would then take the fast
-- path to X's flags and compile K without the binding FOO gates.
unitReplacedMidRequest :: IO FilePath -> TestT IO ()
unitReplacedMidRequest tmp = do
  dir <- liftIO tmp
  let rootX = dir </> "x"
      rootY = dir </> "y"
  state <- liftIO do
    writeRoot rootX [] [Mod {name = k, deps = [], text = kCpp}]
    writeRoot rootY ["-DFOO"] [Mod {name = k, deps = [], text = kCpp}]
    newState
  (stepX, stepY) <- liftIO (interleave state (compileIn state rootX k []) (compileIn state rootY k []))
  kept <- liftIO (keptExports state k)
  stepY2 <- liftIO (compileIn state rootY k [])
  checkSteps [stepX, stepY, stepY2]
  ifaceX <- readIfaceIn rootX k
  ifaceY <- readIfaceIn rootY k
  footnote ("kept after the interleaving: " ++ show kept)
  ifaceX.exports === ["value_1_1"]
  kept === Just ["value_1_1", "value_1_1_foo"]
  ifaceY.exports === ["value_1_1", "value_1_1_foo"]

-- | Commit X's K exports an Int, commit Y's an Integer, and M infers its type from K. Y's restore reloads K from Y's
-- root while X's compile of M has yet to run; X must still compile M against X's K.
dependencyReloadedMidRequest :: IO FilePath -> TestT IO ()
dependencyReloadedMidRequest tmp = do
  dir <- liftIO tmp
  let rootX = dir </> "x"
      rootY = dir </> "y"
      fresh = dir </> "fresh"
      modsFor ty = [Mod {name = k, deps = [], text = kTyped ty}, Mod {name = m, deps = [k], text = mInfers}]
  state <- liftIO do
    writeRoot rootX [] (modsFor "Int")
    writeRoot rootY [] (modsFor "Integer")
    writeRoot fresh [] (modsFor "Int")
    newState
  prep <- liftIO (traverse (\ root -> compileIn state root k []) [rootX, rootY])
  (stepX, stepY) <- liftIO (interleave state (compileIn state rootX m [k]) (compileIn state rootY m [k]))
  freshState <- liftIO newState
  reference <- liftIO (traverse (\ (mo, ds) -> compileIn freshState fresh mo ds) [(k, []), (m, [k])])
  checkSteps (prep ++ [stepX, stepY] ++ reference)
  ifaceX <- readIfaceIn rootX m
  ifaceY <- readIfaceIn rootY m
  ifaceFresh <- readIfaceIn fresh m
  footnote ("X: " ++ show ifaceX ++ ", Y: " ++ show ifaceY ++ ", fresh X: " ++ show ifaceFresh)
  ifaceX.abi === ifaceFresh.abi
  (ifaceY.abi /= ifaceFresh.abi) === True

-- | Commit X's K has the value 1, commit Y's 100, and M's splice runs K. Y's compile links Y's K into whatever
-- interpreter it runs, while X, restored first, has yet to run its splice; X's splice must run X's K.
linkedCodeMidRequest :: IO FilePath -> TestT IO ()
linkedCodeMidRequest tmp = do
  dir <- liftIO tmp
  let rootX = dir </> "x"
      rootY = dir </> "y"
      modsFor n = [Mod {name = k, deps = [], text = kValue n}, Mod {name = m, deps = [k], text = mSplice}]
  state <- liftIO do
    writeRoot rootX [] (modsFor 1)
    writeRoot rootY [] (modsFor 100)
    newState
  prep <- liftIO (traverse (\ root -> compileIn state root k []) [rootX, rootY])
  (stepX, stepY) <- liftIO (interleave state (compileIn state rootX m [k]) (compileIn state rootY m [k]))
  stepY2 <- liftIO (compileIn state rootY m [k])
  checkSteps (prep ++ [stepX, stepY, stepY2])
  ifaceX <- readIfaceIn rootX m
  ifaceY <- readIfaceIn rootY m
  ifaceX.exports === ["spliced_1"]
  ifaceY.exports === ["spliced_100"]

-- | Two builds of one commit in two roots, one after the other, as two isolation dirs or two configurations of one
-- tree give them: the same sources and plan, a different temp dir and, when @argsA@ and @argsB@ differ, a different output
-- directory. The first root is deleted before the second build starts, as Buck deletes an action's scratch and an
-- executor its execution root. The second build must work in its own root and write nothing into the first.
movedRoot :: [String] -> [String] -> FilePath -> IO FilePath -> TestT IO ()
movedRoot argsA argsB outB tmp = do
  dir <- liftIO tmp
  let rootA = dir </> "a"
      rootB = dir </> "b"
      mods = [Mod {name = k, deps = [], text = kValue 1}, Mod {name = m, deps = [k], text = mSplice}]
  state <- liftIO do
    writeRoot rootA argsA mods
    writeRoot rootB argsB mods
    newState
  stepsA <- liftIO (traverse (\ (mo, ds) -> compileIn state rootA mo ds) [(k, []), (m, [k])])
  checkSteps stepsA
  liftIO (removeDirectoryRecursive rootA)
  stepsB <- liftIO (traverse (\ (mo, ds) -> compileIn state rootB mo ds) [(k, []), (m, [k])])
  checkSteps stepsB
  rewritten <- liftIO (doesDirectoryExist rootA)
  footnote ("the deleted root written again: " ++ show rewritten)
  rewritten === False
  written <- liftIO (doesFileExist (rootB </> outB </> m ++ ".dyn_hi"))
  written === True

-- | Two units in one root, laid out as the rules lay them out: unit0 holds the dependency D, unit1 holds M, which
-- re-exports D, so M's interface names the D it was compiled against. Each unit has its own plan, args file and
-- output dir, and unit1's plan lists unit0's plan as its dependency unit.
writeTwoUnits :: FilePath -> FilePath -> FilePath -> ByteString -> IO ()
writeTwoUnits root outDep outMain depText = do
  for_ @[] ["src", "plan" </> "unit0", "plan" </> "unit1", outDep, outMain, "tmp"] \ d ->
    createDirectoryIfMissing True (root </> d)
  ByteString.writeFile (root </> srcPath depMod) depText
  ByteString.writeFile (root </> srcPath mainMod) (hs ["module Unit1Main (module Unit0Dep) where", "import Unit0Dep"])
  writeFile (root </> argsOf "unit0") (unlines (twoUnitArgs ++ ["-this-unit-id", "unit0", "-odir", outDep, "-hidir", outDep]))
  writeFile (root </> argsOf "unit1")
    (unlines (twoUnitArgs ++ ["-this-unit-id", "unit1", "-package-id", "unit0", "-odir", outMain, "-hidir", outMain]))
  Aeson.encodeFile (root </> planOf "unit0") (planFor "unit0" depMod [] Nothing)
  Aeson.encodeFile (root </> depUnitsOf "unit1")
    (CachedBuildPlans [CachedBuildPlan {name = JsonFs (stringToUnitId "unit0"), build_plan = toOsPath (planOf "unit0")}])
  Aeson.encodeFile (root </> planOf "unit1")
    (planFor "unit1" mainMod [CachedPackageDep {id = JsonFs (stringToUnitId "unit0"), modules = [JsonFs (mkModuleName depMod)]}]
      (Just (toOsPath (depUnitsOf "unit1"))))
  where
    planFor unit name packages depUnits =
      CachedUnit {
        build_plan = Just (Map.singleton (JsonFs (mkModuleName name)) CachedModule {
          source = toOsPath (srcPath name),
          modules = [],
          packages,
          flags = []
        }),
        cache = Nothing,
        is_binary = False,
        unit_args = Just (toOsPath (argsOf unit)),
        unit_buck_args = Nothing,
        dep_units = depUnits
      }

    twoUnitArgs = [
      "-i",
      "-hide-all-packages",
      "-package", "base",
      "-dynamic",
      "-fbyte-code-and-object-code",
      "-fprefer-byte-code",
      "-fPIC",
      "-osuf", "dyn_o",
      "-hisuf", "dyn_hi"
      ]

depMod, mainMod :: String
depMod = "Unit0Dep"
mainMod = "Unit1Main"

planOf, argsOf, depUnitsOf :: String -> FilePath
planOf unit = "plan" </> unit </> "cached_unit.json"
argsOf unit = "plan" </> unit </> "unit_args"
depUnitsOf unit = "plan" </> unit </> "dep_units.json"

-- | 'compileIn' for a module of either unit in a 'writeTwoUnits' root.
compileUnitIn :: MVar WorkerState -> FilePath -> String -> String -> [(String, String)] -> IO Step
compileUnitIn state root unit modName deps = do
  lock <- newProcessCwdLock
  withTestLog False label \ (log, logVar) -> do
    result <- try $ withRequestCwd lock (CommandEnv (Map.singleton requestCwdVar root)) False do
      let env = Env {log, state, args}
      compiled <- withGhcMakeModule Compiled target env \ _ -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        compileModuleWithDepsInHpt log (TargetModule target)
      pure (isJust compiled)
    TestLog {diagnostics, fatal} <- readIORef logVar
    let logged = [d.rendered | d <- diagnostics] ++ fatal
    pure case result of
      Right ok -> Step {label, ok, output = logged}
      Left (e :: SomeException) -> Step {label, ok = False, output = logged ++ [displayException e]}
  where
    label = modName ++ " in " ++ root

    target = ModuleTarget {mod = mkModule (stringToUnit unit) (mkModuleName modName)}

    args =
      (emptyArgs []) {
        homeUnit = Just (toOsPath (planOf unit)),
        cachedDeps = Just (CachedDeps [CachedDep {name = JsonFs (mkModuleName d), package = JsonFs (stringToUnitId u)} | (d, u) <- deps]),
        tempDir = Just (toOsPath (root </> "tmp")),
        -- As the ghcw servers run: dependency units restored on other threads would read the plans' relative paths
        -- from the process's working directory rather than the request's.
        features = defaultFeatureFlags {concurrentInitUnits = False}
      }

-- | A dependency's output dir moves by its configuration segment between two builds in one root, and its source gains
-- an export. Another worker compiles the dependency into its new dir, as an executor or a cache hit would, and the
-- long-lived worker is asked for the dependent alone. The dependent must be compiled against the new dependency, read
-- from the new dir. The old dir is left in place, so reading it is a silent wrong answer rather than a crash.
movedDependency :: FilePath -> FilePath -> IO FilePath -> TestT IO ()
movedDependency mainOutA mainOutB tmp = do
  dir <- liftIO tmp
  let root = dir </> "r"
      depA = hs ["module Unit0Dep where", "dep_1 :: Int", "dep_1 = 1"]
      depB = hs ["module Unit0Dep where", "dep_1 :: Int", "dep_1 = 1", "dep_2 :: Int", "dep_2 = 2"]
  long <- liftIO newState
  other <- liftIO newState
  liftIO (writeTwoUnits root ("out" </> "cfg-a" </> "unit0") mainOutA depA)
  first <- liftIO (sequence [
    compileUnitIn long root "unit0" depMod [],
    compileUnitIn long root "unit1" mainMod [(depMod, "unit0")]
    ])
  checkSteps first
  liftIO (writeTwoUnits root ("out" </> "cfg-b" </> "unit0") mainOutB depB)
  elsewhere <- liftIO (compileUnitIn other root "unit0" depMod [])
  checkSteps [elsewhere]
  dependent <- liftIO (compileUnitIn long root "unit1" mainMod [(depMod, "unit0")])
  checkSteps [dependent]
  iface <- readIfaceAt (root </> mainOutB </> mainMod ++ ".dyn_hi")
  footnote ("the dependent's exports: " ++ show iface.exports)
  iface.exports === ["dep_1", "dep_2"]

readIfaceAt :: FilePath -> TestT IO Iface
readIfaceAt path =
  transientSession [] do
    hsc_env@HscEnv {hsc_dflags, hsc_NC} <- getSession
    iface <- liftIO (readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace path)
    pure Iface {exports = sort (concatMap (map getOccString . availNames) (mi_exports iface)), abi = showAbiHash hsc_env iface}

-- | Two units laid out the way a Temporal workflow and the spec that runs it are in mercury-web-backend: unit0 holds
-- Unit0Other and Unit0Workflow, which imports it and declares @Impl_A@; unit1 holds Unit1Registry, which lists
-- @''Impl_A@ as a TH 'Language.Haskell.TH.Name', and Unit1Spec, whose splice turns each listed name into a type, as
-- @$$(provideSmallTemporalWorker)@ does. The splice's output names Impl_A by its original name, so GHC finds it by
-- reading Unit0Workflow's interface from whatever the server keeps for unit0 when the splice runs.
writeThUnits :: FilePath -> ByteString -> IO ()
writeThUnits root otherText = writeThUnitsNamed root otherText ["Impl_A"]

-- | 'writeThUnits' with the workflow module declaring @names@ and the registry listing every one of them, so builds
-- whose registries list different Names make the spec's splice return different declarations.
writeThUnitsNamed :: FilePath -> ByteString -> [ByteString] -> IO ()
writeThUnitsNamed root otherText names = do
  for_ @[] ["src", "plan" </> "unit0", "plan" </> "unit1", "out" </> "unit0", "out" </> "unit1", "tmp"] \ d ->
    createDirectoryIfMissing True (root </> d)
  ByteString.writeFile (root </> srcPath otherMod) otherText
  ByteString.writeFile (root </> srcPath workflowMod)
    (hs (["module Unit0Workflow (" <> ByteString.intercalate ", " [n <> " (..)" | n <- names] <> ", workflow_1) where", "import Unit0Other"]
      ++ ["data " <> n <> " = " <> n | n <- names]
      ++ ["workflow_1 :: Int", "workflow_1 = other_1"]))
  ByteString.writeFile (root </> srcPath registryMod)
    (hs [
      "{-# LANGUAGE TemplateHaskell #-}",
      "module Unit1Registry (registry) where",
      "import Language.Haskell.TH (Name)",
      "import Unit0Workflow (" <> ByteString.intercalate ", " names <> ")",
      "registry :: [Name]",
      "registry = [" <> ByteString.intercalate ", " ["''" <> n | n <- names] <> "]"
      ])
  ByteString.writeFile (root </> srcPath specMod)
    (hs [
      "{-# LANGUAGE TemplateHaskell #-}",
      "module Unit1Spec where",
      "import Language.Haskell.TH (conT, mkName, tySynD)",
      "import Unit1Registry (registry)",
      "$(traverse (\\ (i, n) -> tySynD (mkName (\"Worker\" ++ show i)) [] (conT n)) (zip [1 :: Int ..] registry))"
      ])
  writeFile (root </> argsOf "unit0")
    (unlines (thArgs ++ ["-this-unit-id", "unit0", "-odir", "out" </> "unit0", "-hidir", "out" </> "unit0"]))
  writeFile (root </> argsOf "unit1")
    (unlines (thArgs ++ ["-this-unit-id", "unit1", "-package-id", "unit0", "-odir", "out" </> "unit1", "-hidir", "out" </> "unit1"]))
  Aeson.encodeFile (root </> planOf "unit0") (unitPlan "unit0" [(otherMod, []), (workflowMod, [otherMod])] [] Nothing)
  Aeson.encodeFile (root </> depUnitsOf "unit1")
    (CachedBuildPlans [CachedBuildPlan {name = JsonFs (stringToUnitId "unit0"), build_plan = toOsPath (planOf "unit0")}])
  Aeson.encodeFile (root </> planOf "unit1")
    (unitPlan "unit1" [(registryMod, []), (specMod, [registryMod])]
      [CachedPackageDep {id = JsonFs (stringToUnitId "unit0"), modules = JsonFs . mkModuleName <$> [otherMod, workflowMod]}]
      (Just (toOsPath (depUnitsOf "unit1"))))
  where
    unitPlan unit mods packages depUnits =
      CachedUnit {
        build_plan = Just (Map.fromList [
          (JsonFs (mkModuleName name), CachedModule {
            source = toOsPath (srcPath name),
            modules = JsonFs . mkModuleName <$> deps,
            packages,
            flags = []
          })
          | (name, deps) <- mods
          ]),
        cache = Nothing,
        is_binary = False,
        unit_args = Just (toOsPath (argsOf unit)),
        unit_buck_args = Nothing,
        dep_units = depUnits
      }

    thArgs = [
      "-i",
      "-hide-all-packages",
      "-package", "base",
      "-package", "template-haskell",
      "-dynamic",
      "-fbyte-code-and-object-code",
      "-fprefer-byte-code",
      "-fPIC",
      "-osuf", "dyn_o",
      "-hisuf", "dyn_hi"
      ]

otherMod, workflowMod, registryMod, specMod :: String
otherMod = "Unit0Other"
workflowMod = "Unit0Workflow"
registryMod = "Unit1Registry"
specMod = "Unit1Spec"

-- | The four compiles of one build of 'writeThUnits', in dependency order, on one server.
thBuild :: MVar WorkerState -> FilePath -> IO [Step]
thBuild state root =
  sequence [
    compileUnitIn state root "unit0" otherMod [],
    compileUnitIn state root "unit0" workflowMod [(otherMod, "unit0")],
    compileUnitIn state root "unit1" registryMod [(workflowMod, "unit0"), (otherMod, "unit0")],
    thSpecIn state root
    ]

thSpecIn :: MVar WorkerState -> FilePath -> IO Step
thSpecIn state root =
  compileUnitIn state root "unit1" specMod [(registryMod, "unit1"), (workflowMod, "unit0"), (otherMod, "unit0")]

-- | What gogc-before and gogc-after hit on c1's bzlw servers on 2026-10-12 and 13 (GHC-83249 "Can't find
-- interface-file declaration for type constructor or class ...Impl_..." in the result of
-- @$$(provideSmallTemporalWorker)@, beside "reload M: source hash changed"): about 37 builds of one tree, each with a
-- synthetic edit that adds an exported binding to a few modules, share servers, and no edit touches the module that
-- declares the missing type. Here build Y adds a binding to Unit0Other, below Unit0Workflow in the same unit, so Y's
-- requests reload unit0 by source and ABI hash. Y's low unit and registry are compiled on another server, as another
-- executor or a cache hit gives them. On the long server X's spec request is held at the end of its restore while
-- all of Y's spec request runs; then X's splice must still find Impl_A, and so must a later request of either build.
thSpliceReloadedMidRequest :: Bool -> IO FilePath -> TestT IO ()
thSpliceReloadedMidRequest yFirst tmp = do
  dir <- liftIO tmp
  let rootX = dir </> "x"
      rootY = dir </> "y"
      otherX = hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1"]
      otherY = hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1", "labStress_1 :: ()", "labStress_1 = ()"]
  long <- liftIO newState
  other <- liftIO newState
  liftIO do
    writeThUnits rootX otherX
    writeThUnits rootY otherY
  first <- liftIO (thBuild long rootX)
  checkSteps first
  elsewhere <- liftIO (sequence [
    compileUnitIn other rootY "unit0" otherMod [],
    compileUnitIn other rootY "unit0" workflowMod [(otherMod, "unit0")],
    compileUnitIn other rootY "unit1" registryMod [(workflowMod, "unit0"), (otherMod, "unit0")]
    ])
  checkSteps elsewhere
  let (heldRoot, runRoot) = if yFirst then (rootY, rootX) else (rootX, rootY)
  (stepHeld, stepRun) <- liftIO (interleave long (thSpecIn long heldRoot) (thSpecIn long runRoot))
  after <- liftIO (traverse (thSpecIn long) [rootX, rootY, rootX])
  checkSteps ([stepHeld, stepRun] ++ after)
  for_ @[] [rootX, rootY] \ root -> do
    spec <- readIfaceAt (root </> "out" </> "unit1" </> specMod ++ ".dyn_hi")
    footnote (root ++ ": spec exports " ++ show spec.exports)
    spec.exports === ["Worker1"]


-- | One build of 'writeThUnits' whose low unit and registry another server compiled, as executors and cache hits
-- give them on c1: the long server is asked only for the spec.
thElsewhere :: MVar WorkerState -> FilePath -> IO [Step]
thElsewhere other root =
  sequence [
    compileUnitIn other root "unit0" otherMod [],
    compileUnitIn other root "unit0" workflowMod [(otherMod, "unit0")],
    compileUnitIn other root "unit1" registryMod [(workflowMod, "unit0"), (otherMod, "unit0")]
    ]

-- | The c1 mode, after hs-worker-i's reading: a bzlw server takes one request at a time (BazelWorker.hs reads,
-- handles and answers each WorkRequest before the next), and every runner that failed with GHC-83249 had served
-- 3 to 12 other builds first. So the state is carried from request to request, not shared within one. Here one
-- long server builds A whole, then serves spec requests for builds B, C and A again in @order@, where each of B and
-- C added a different exported binding below the workflow module and had its low unit and registry compiled on
-- another server. Every spec request must find Impl_A and produce one alias.
thSpliceSequentialHistory :: [Char] -> IO FilePath -> TestT IO ()
thSpliceSequentialHistory order tmp = do
  dir <- liftIO tmp
  let rootOf b = dir </> [b]
      otherText b = case b of
        'b' -> hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1", "labStress_b :: ()", "labStress_b = ()"]
        'c' -> hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1", "labStress_c :: ()", "labStress_c = ()"]
        _ -> hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1"]
  long <- liftIO newState
  other <- liftIO newState
  liftIO (for_ @[] "abc" \ b -> writeThUnits (rootOf b) (otherText b))
  first <- liftIO (thBuild long (rootOf 'a'))
  checkSteps first
  elsewhere <- liftIO (concat <$> traverse @[] (thElsewhere other . rootOf) "bc")
  checkSteps elsewhere
  specs <- liftIO (traverse (thSpecIn long . rootOf) order)
  checkSteps specs
  for_ @[] "abc" \ b -> do
    spec <- readIfaceAt (rootOf b </> "out" </> "unit1" </> specMod ++ ".dyn_hi")
    spec.exports === ["Worker1"]


-- | 'thSpliceSequentialHistory' with registries that differ between builds, as hs-worker-i suggested: the c1 error is
-- in the splice's result, and a registry whose Names differ changes what the splice returns. Build A's workflow
-- module declares Impl_A and its registry lists it; build B's declares and lists Impl_A and Impl_B. B's low unit and
-- registry come from another server. One sequential server builds A whole, then serves spec requests in @order@;
-- each spec must produce exactly one alias per Name its own registry lists.
thSpliceRegistryHistory :: [Char] -> IO FilePath -> TestT IO ()
thSpliceRegistryHistory order tmp = do
  dir <- liftIO tmp
  let rootOf b = dir </> [b]
      namesOf b = if b == 'b' then ["Impl_A", "Impl_B"] else ["Impl_A"]
      other = hs ["module Unit0Other where", "other_1 :: Int", "other_1 = 1"]
  long <- liftIO newState
  elsewhereState <- liftIO newState
  liftIO (for_ @[] "ab" \ b -> writeThUnitsNamed (rootOf b) other (namesOf b))
  first <- liftIO (thBuild long (rootOf 'a'))
  checkSteps first
  elsewhere <- liftIO (thElsewhere elsewhereState (rootOf 'b'))
  checkSteps elsewhere
  specs <- liftIO (traverse @[] (thSpecIn long . rootOf) order)
  checkSteps specs
  specA <- readIfaceAt (rootOf 'a' </> "out" </> "unit1" </> specMod ++ ".dyn_hi")
  specB <- readIfaceAt (rootOf 'b' </> "out" </> "unit1" </> specMod ++ ".dyn_hi")
  footnote ("A: " ++ show specA.exports ++ ", B: " ++ show specB.exports)
  specA.exports === ["Worker1"]
  specB.exports === ["Worker1", "Worker2"]


test_concurrent :: TestTree
test_concurrent =
  testGroup "two commits interleaved on one server" [
    withTemp "concurrent-unit" \ tmp ->
      unitTest "a unit replaced by a later request is not written back over" (unitReplacedMidRequest tmp),
    withTemp "concurrent-dep" \ tmp ->
      unitTest "a dependency reloaded by a later request does not reach an earlier compile" (dependencyReloadedMidRequest tmp),
    withTemp "concurrent-th" \ tmp ->
      unitTest "code linked by a later request does not run in an earlier splice" (linkedCodeMidRequest tmp),
    withTemp "moved-root-tmp" \ tmp ->
      unitTest "a build in another root does not use the deleted first root's temp dir" (movedRoot [] [] "out" tmp),
    withTemp "moved-root-out" \ tmp ->
      unitTest "a build whose output directory moved writes under its own" (movedRoot [] ["-odir", "out-b", "-hidir", "out-b"] "out-b" tmp),
    withTemp "moved-root-cfg" \ tmp ->
      unitTest "a build whose output path differs only in its configuration segment writes under its own" $
        movedRoot
          ["-odir", "out/cfg-0123", "-hidir", "out/cfg-0123"]
          ["-odir", "out/cfg-4567", "-hidir", "out/cfg-4567"]
          ("out" </> "cfg-4567")
          tmp,
    withTemp "moved-dep-both" \ tmp ->
      unitTest "a dependency and its dependent move configuration: the dependent compiles against the moved dependency" $
        movedDependency ("out" </> "cfg-a" </> "unit1") ("out" </> "cfg-b" </> "unit1") tmp,
    withTemp "th-splice-reloaded" \ tmp ->
      unitTest "a splice naming a dependency unit's type, held while another build reloads that unit: it finds the type" $
        thSpliceReloadedMidRequest False tmp,
    withTemp "th-splice-reloaded-y" \ tmp ->
      unitTest "a splice naming a dependency unit's type, the edited build held: it finds the type" $
        thSpliceReloadedMidRequest True tmp,
    withTemp "th-splice-history-bca" \ tmp ->
      unitTest "a splice naming a dependency unit's type, after other builds on one sequential server (b c a): it finds the type" $
        thSpliceSequentialHistory "bca" tmp,
    withTemp "th-splice-history-babcab" \ tmp ->
      unitTest "a splice naming a dependency unit's type, builds alternating on one sequential server (b a b c a b): it finds the type" $
        thSpliceSequentialHistory "babcab" tmp,
    withTemp "th-registry-history-ba" \ tmp ->
      unitTest "a splice over a registry that differs by build, on one sequential server (b a): each spec gets its own Names" $
        thSpliceRegistryHistory "ba" tmp,
    withTemp "th-registry-history-babab" \ tmp ->
      unitTest "a splice over a registry that differs by build, alternating on one sequential server (b a b a b): each spec gets its own Names" $
        thSpliceRegistryHistory "babab" tmp
  ]
