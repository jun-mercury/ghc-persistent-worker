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
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import System.Timeout (timeout)
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Log (withTestLog)
import Test.Run (transientSession, unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..), emptyArgs)
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.CachedDeps (CachedDep (..), CachedDeps (..), CachedModule (..), CachedUnit (..), JsonFs (..))
import Types.Env (Env (..))
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

test_concurrent :: TestTree
test_concurrent =
  testGroup "two commits interleaved on one server" [
    withTemp "concurrent-unit" \ tmp ->
      unitTest "a unit replaced by a later request is not written back over" (unitReplacedMidRequest tmp),
    withTemp "concurrent-dep" \ tmp ->
      unitTest "a dependency reloaded by a later request does not reach an earlier compile" (dependencyReloadedMidRequest tmp),
    withTemp "concurrent-th" \ tmp ->
      unitTest "code linked by a later request does not run in an earlier splice" (linkedCodeMidRequest tmp)
  ]
