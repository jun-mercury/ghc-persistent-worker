module ExtraLibTest where

import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import GHC (DynFlags (..), getSession, getSessionDynFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Unit (UnitId, stringToUnitId)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..))
import GHC.Unit.Home.Graph (UnitEnvGraph (..))
import GHC.Utils.CliOption (Option (..))
import Hedgehog (TestT, (===))
import Internal.Cache.Metadata (loadCachedDepUnits)
import qualified Internal.State.Make as Make
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import Test.Run (mkEnv, persistentSession, unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedUnit (..), JsonFs (..))
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..), defaultFeatureFlags)
import Types.State (WorkerState (..))
import Types.State.Make (LibLoadState (..), MakeState (..))

-- | Write the args file and the plan of a dependency unit with no modules, the way the metadata step does.
writeUnit :: FilePath -> String -> [String] -> IO CachedBuildPlan
writeUnit tmp name libArgs = do
  writeFile args (unlines (["-hide-all-packages", "-package", "base", "-this-unit-id", name] ++ libArgs))
  Aeson.encodeFile plan CachedUnit {
    build_plan = Just Map.empty,
    cache = Nothing,
    is_binary = False,
    unit_args = Just (toOsPath args),
    unit_buck_args = Nothing,
    dep_units = Nothing
  }
  pure CachedBuildPlan {name = JsonFs (stringToUnitId name), build_plan = toOsPath plan}
  where
    args = tmp </> name ++ ".args"
    plan = tmp </> name ++ ".json"

-- | The library search paths and link inputs a home unit's flags name.
linkInputs :: UnitId -> UnitEnvGraph HomeUnitEnv -> Maybe ([FilePath], [String])
linkInputs unit (UnitEnvGraph graph) = do
  ue <- Map.lookup unit graph
  pure (libraryPaths ue.homeUnitEnv_dflags, [o | Option o <- ldInputs ue.homeUnitEnv_dflags])

-- | Restore two dependency units from cache, one whose args name a native library and one whose args name none,
-- then write the session back as a compile request does.
--
-- The libraries go to the state for 'Internal.State.Linkables.ensureLibraries', unit by unit, and the stored unit
-- envs name none, so a later request in another execution root does not make GHC's loader initialisation load them.
-- The session the units were restored in keeps them.
restoreTwoUnits :: Bool -> IO FilePath -> TestT IO ()
restoreTwoUnits fastParser tmpResource = do
  tmp <- liftIO tmpResource
  withLib <- liftIO (writeUnit tmp "dep-with-lib" ["-L" ++ tmp </> "with", "-lwith"])
  plain <- liftIO (writeUnit tmp "dep-plain" [])
  (env, _) <- liftIO mkEnv
  let
    features = defaultFeatureFlags {flagParser = fastParser, concurrentInitUnits = False}
    plans = CachedBuildPlans [withLib, plain]
    requestArgs = ["-hide-all-packages", "-package", "base", "-this-unit-id", "requester"]
  (restored, stored, session) <- persistentSession env.state requestArgs do
    dflags0 <- getSessionDynFlags
    hsc_env <- getSession
    liftIO $ modifyMVar env.state \ state0 -> do
      (state1, hsc_env1) <- loadCachedDepUnits env.log dflags0 plans features (state0, hsc_env)
      stored <- Make.storeState env.log hsc_env1 state1.make
      pure (state1, (state1.make, stored, hsc_env1.hsc_unit_env.ue_home_unit_graph))
  Map.lookup withLibId restored.extraLib.requested === Just ([tmp </> "with"], ["with"])
  Map.lookup plainId restored.extraLib.requested === Just ([], [])
  noLinkInputs restored.hug
  noLinkInputs stored.hug
  linkInputs withLibId session === Just ([tmp </> "with"], ["-lwith"])
  linkInputs plainId session === Just ([], [])
  where
    withLibId = stringToUnitId "dep-with-lib"
    plainId = stringToUnitId "dep-plain"

    noLinkInputs hug = do
      linkInputs withLibId hug === Just ([], [])
      linkInputs plainId hug === Just ([], [])

test_extraLibs :: TestTree
test_extraLibs =
  testGroup "native libraries of stored units" [
    withTemp "extra-libs-ghc-parser" \ tmp ->
      unitTest "a stored unit names no libraries and the state records its own, with GHC's flag parser" (restoreTwoUnits False tmp),
    withTemp "extra-libs-fast-parser" \ tmp ->
      unitTest "a stored unit names no libraries and the state records its own, with the fast flag parser" (restoreTwoUnits True tmp)
  ]
