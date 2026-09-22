{-# LANGUAGE CPP #-}

module Internal.State.Make where

import Control.Concurrent.MVar (readMVar)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC (Module, ModuleName)
import GHC.Driver.Env (HscEnv (..))
import GHC.Linker.Types (Loader (..), LoaderState (..))
import GHC.Runtime.Interpreter.Types (Interp (..))
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (UnitEnvGraph (..), lookupHugUnit, unitEnv_insert, unitEnv_lookup)
import GHC.Unit.Module.Env (lookupModuleEnv)
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraph,
  ModuleGraphNode (..),
  NodeKey (..),
  mgModSummaries',
  mkNodeKey,
  mnkUnitId,
  )
import GHC.Unit.Types (GenWithIsBoot (..), UnitId, instUnitInstanceOf)
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.Compat.ModuleGraph (mkModuleGraph)
import Internal.State.Stats (logMemStats)
import Internal.State.UnitIndex (restoreUnitIndex)
import Types.Log (Logger (..))
import Types.State.Make (LibLoadState (..), MakeState (..), UnitFingerprint)

-- | Restore the shared state used by both @computeMetadata@ and @compileHpt@ from the cache.
-- See 'loadCacheMakeCompile' for details.
loadState ::
  HscEnv ->
  MakeState ->
  HscEnv
loadState hsc_env state =
  restoreUnitIndex state (restoreHug (restoreModuleGraph hsc_env))
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    restoreModuleGraph e = e {hsc_unit_env = e.hsc_unit_env {ue_module_graph = state.moduleGraph}}
#else
    restoreModuleGraph e = e {hsc_mod_graph = state.moduleGraph}
#endif

    restoreHug e = e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = state.hug}}

-- | Reuse the interpreter stored from an earlier session, or adopt the one the current session initialized when it
-- parsed its flags. 'hsc_interp' is only set once the session has parsed its flags, so the first session's is the one
-- kept. Callers run 'loadState', then their own setup (which may 'evictUnit' or drop the interpreter), then this, so a
-- decision made during setup governs the interpreter this session compiles with, not only the next one's.
ensureInterp ::
  HscEnv ->
  MakeState ->
  (MakeState, HscEnv)
ensureInterp hsc_env state =
  maybe storeInterp restoreInterp state.interp
  where
    storeInterp = (state {interp = hsc_env.hsc_interp}, hsc_env)

    restoreInterp interp = (state, hsc_env {hsc_interp = Just interp})

nodeKeyUnit :: NodeKey -> Maybe UnitId
nodeKeyUnit = \case
  NodeKey_Module k -> Just (mnkUnitId k)
  NodeKey_Link uid -> Just uid
  NodeKey_Unit iu -> Just (instUnitInstanceOf iu)

graphModules :: UnitId -> ModuleGraph -> Set ModuleName
graphModules unit graph =
  Set.fromList [gwib_mod (mnkModuleName k) | node <- mgModSummaries' graph, NodeKey_Module k <- [mkNodeKey node], mnkUnitId k == unit]

-- | Forget everything the worker stored for a unit, so the next request for it restores it from its plan as if the
-- worker had never seen it: its 'HomeUnitEnv', its module graph nodes and the derived graph, its bytecode load locks,
-- its extra-library record and its fingerprint. The interpreter goes too, because its loader may hold the unit's code
-- and GHC never relinks a module it has already loaded; the next session reinitializes it and relinks from the current
-- interfaces, at the cost of one relink.
evictUnit :: UnitId -> MakeState -> MakeState
evictUnit uid state =
  (rebuildModuleGraph state {
    hug = deleteUnitEnv uid state.hug,
    moduleGraphNodes = kept,
    bcoLoadState = foldr Map.delete state.bcoLoadState droppedNames,
    extraLib = state.extraLib {requested = Map.delete uid state.extraLib.requested},
    unitFingerprints = Map.delete uid state.unitFingerprints
  }) {interp = Nothing}
  where
    (dropped, kept) = Map.partitionWithKey (\ k _ -> nodeKeyUnit k == Just uid) state.moduleGraphNodes
    droppedNames = [gwib_mod (mnkModuleName k) | NodeKey_Module k <- Map.keys dropped]

deleteUnitEnv :: UnitId -> UnitEnvGraph v -> UnitEnvGraph v
deleteUnitEnv uid (UnitEnvGraph m) = UnitEnvGraph (Map.delete uid m)

storeUnitFingerprint :: UnitId -> UnitFingerprint -> MakeState -> MakeState
storeUnitFingerprint uid fp state =
  state {unitFingerprints = Map.insert uid fp state.unitFingerprints}

knownUnit :: UnitId -> MakeState -> Bool
knownUnit uid state = isJust (lookupHugUnit uid state.hug)

-- | Whether the stored interpreter has linked this module's code. If it has, recompiling or reloading the module would
-- leave its old code shadowing the new inside a later splice, since GHC's @getLinkDeps@ skips modules already loaded.
linkedInInterp :: Module -> MakeState -> IO Bool
linkedInInterp modu state =
  case state.interp of
    Nothing -> pure False
    Just interp ->
      readMVar (loader_state (interpLoader interp)) <&> \case
        Nothing -> False
        Just ls -> isJust (lookupModuleEnv (bcos_loaded ls) modu) || isJust (lookupModuleEnv (objs_loaded ls) modu)

dropInterpIfLinked :: Logger -> Module -> MakeState -> IO MakeState
dropInterpIfLinked logger modu state = do
  linked <- linkedInInterp modu state
  if linked
    then do
      logger.info ("ghc-worker: drop interpreter: " ++ showPprUnsafe modu ++ " is loaded and about to change")
      pure state {interp = Nothing}
    else pure state

-- | Merge the given nodes into the cached node index, leaving the derived 'moduleGraph' untouched.
--
-- In more recent versions of GHC, the function for merging graphs is not exposed anymore.
-- There was also some issue with node duplication, which is why this function is so convoluted.
mergeModuleGraphNodes :: [ModuleGraphNode] -> Map.Map NodeKey ModuleGraphNode -> Map.Map NodeKey ModuleGraphNode
mergeModuleGraphNodes new oldMap = merged
  where
    !merged = Map.unionWith mergeNodes oldMap newMap

    mergeNodes = \cases
      old@(ModuleNode _oldDeps _oldSumm) (ModuleNode _newDeps _newSumm) -> old
      _ newNode -> newNode

    newMap = Map.fromList $ [(mkNodeKey n, n) | n <- new]

storeModuleGraphNodes :: [ModuleGraphNode] -> MakeState -> MakeState
storeModuleGraphNodes new state =
  state {moduleGraphNodes = merged}
  where
    !merged = mergeModuleGraphNodes new state.moduleGraphNodes

-- | Derive 'moduleGraph' from the node index.
--
-- This is @O(size of the index)@, so when a batch of units is restored it must be called once for the batch rather than
-- once per unit.
rebuildModuleGraph :: MakeState -> MakeState
rebuildModuleGraph state =
  state {moduleGraph = mkModuleGraph (Map.elems state.moduleGraphNodes)}

-- | Merge the given module graph into the cached graph and derive 'moduleGraph' immediately.
storeModuleGraph :: ModuleGraph -> MakeState -> MakeState
storeModuleGraph new =
  rebuildModuleGraph . storeModuleGraphNodes (mgModSummaries' new)

-- | Extract the unit env of the currently active unit and store it in the cache.
-- This is used by the make mode worker after the metadata step has initialized the new unit.
insertUnitEnv :: HscEnv -> MakeState -> MakeState
insertUnitEnv hsc_env state =
  state {hug = update state.hug}
  where
    ue = unitEnv_lookup current hsc_env.hsc_unit_env.ue_home_unit_graph
    current = hsc_env.hsc_unit_env.ue_current_unit
    update = unitEnv_insert current ue

-- | Store the changes made to the HUG by @compileHpt@ in the state, which usually consists of adding a single
-- 'HomeModInfo'.
storeState ::
  Logger ->
  HscEnv ->
  MakeState ->
  IO MakeState
storeState logger hsc_env state = do
  logMemStats "store make state" logger
  pure state {hug}
  where
    !hug = UnitEnvGraph (new <> old)

    UnitEnvGraph !new = hsc_env.hsc_unit_env.ue_home_unit_graph

    UnitEnvGraph !old = state.hug
