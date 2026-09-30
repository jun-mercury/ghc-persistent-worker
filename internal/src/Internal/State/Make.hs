{-# LANGUAGE CPP #-}

module Internal.State.Make where

import Control.Concurrent.MVar (readMVar)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef)
import Data.IntMap qualified as IM
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC (ModIface, ModuleName)
import GHC.Driver.DynFlags (DynFlags (..))
import GHC.Driver.Env (HscEnv (..))
import GHC.Fingerprint (fingerprintFingerprints, fingerprintString)
import GHC.Linker.Types (Loader (..), LoaderState (..))
import GHC.Runtime.Interpreter.Types (Interp (..))
import GHC.Types.Unique.DFM (eltsUDFM, emptyUDFM, lookupUDFM)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..))
import GHC.Unit.Home.Graph (UnitEnvGraph (..), lookupHugUnit, unitEnv_insert, unitEnv_lookup, unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), homeModInfoByteCode, homeModInfoObject)
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, hptInternalTableFromRef, hptInternalTableRef)
import GHC.Unit.Module.Env (DModuleNameEnv, moduleEnvKeys)
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraph,
  ModuleGraphNode (..),
  NodeKey (..),
  mgModSummaries',
  mkNodeKey,
  mnkUnitId,
  )
import GHC.Unit.Module.Graph qualified as GHC.MG (mkModuleGraph)
import GHC.Unit.Module.ModIface (mi_module, mi_src_hash, mi_mod_hash)
import GHC.Unit.Types (GenModule (..), GenWithIsBoot (..), UnitId, instUnitInstanceOf, toUnitId)
import GHC.Utils.CliOption (Option (..))
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.State.Stats (logMemStats)
import Internal.State.UnitIndex (restoreUnitIndex)
import Types.Log (Logger (..))
import Types.State.Make (
  CodeVersion,
  EModuleGraph (..),
  HomeModuleKey,
  InterpPool (..),
  KeyIndexNodeMap (..),
  LibLoadState (..),
  MakeState (..),
  SharedInterp (..),
  UnitFingerprint,
  emptyEModuleGraph,
  )

#if !MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.ModIface (mi_final_exts, mi_flag_hash)

#endif

import Internal.Compat.ModuleGraph qualified as MG

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
    restoreModuleGraph e = e {hsc_unit_env = e.hsc_unit_env {ue_module_graph = state.moduleGraphState.moduleGraph}}
#else
    restoreModuleGraph e = e {hsc_mod_graph = state.moduleGraphState.moduleGraph}
#endif

    restoreHug e = e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = state.hug}}

nodeKeyUnit :: NodeKey -> Maybe UnitId
nodeKeyUnit = \case
  NodeKey_Module k -> Just (mnkUnitId k)
  NodeKey_Link uid -> Just uid
  NodeKey_Unit iu -> Just (instUnitInstanceOf iu)

graphModules :: UnitId -> ModuleGraph -> Set ModuleName
graphModules unit graph =
  Set.fromList [gwib_mod (mnkModuleName k) | node <- mgModSummaries' graph, NodeKey_Module k <- [mkNodeKey node], mnkUnitId k == unit]

-- | Forget everything the worker stored for a unit, so the next request for it restores it from its plan as if the
-- worker had never seen it: its 'HomeUnitEnv' and generation, its module graph nodes and the derived graph, its
-- bytecode load locks, its extra-library record and its fingerprint. The interpreters keep what they linked: a
-- request whose dependency closure has other code for one of the unit's modules no longer agrees with them and gets an
-- interpreter of its own, see 'SharedInterp'.
evictUnit :: Bool -> UnitId -> MakeState -> MakeState
evictUnit useIncr uid state =
  rebuildModuleGraph useIncr state {
    -- The incremental reachability index only grows, so the derived graph is rebuilt from the kept nodes.
    moduleGraphState = emptyEModuleGraph,
    hug = deleteUnitEnv uid state.hug,
    moduleGraphNodes = kept,
    bcoLoadState = Map.filterWithKey (\ (u, _) _ -> u /= uid) state.bcoLoadState,
    extraLib = state.extraLib {requested = Map.delete uid state.extraLib.requested},
    unitFingerprints = Map.delete uid state.unitFingerprints,
    unitGenerations = Map.delete uid state.unitGenerations
  }
  where
    kept = Map.filterWithKey (\ k _ -> nodeKeyUnit k /= Just uid) state.moduleGraphNodes

deleteUnitEnv :: UnitId -> UnitEnvGraph v -> UnitEnvGraph v
deleteUnitEnv uid (UnitEnvGraph m) = UnitEnvGraph (Map.delete uid m)

storeUnitFingerprint :: UnitId -> UnitFingerprint -> MakeState -> MakeState
storeUnitFingerprint uid fp state =
  state {unitFingerprints = Map.insert uid fp state.unitFingerprints}

knownUnit :: UnitId -> MakeState -> Bool
knownUnit uid state = isJust (lookupHugUnit uid state.hug)

-- | Merge the given nodes into the cached node index, leaving the derived 'moduleGraph' untouched.
--
-- In more recent versions of GHC, the function for merging graphs is not exposed anymore.
-- There was also some issue with node duplication, which is why this function is so convoluted.
mergeModuleGraphNodes ::
  [ModuleGraphNode] ->
  Map.Map NodeKey ModuleGraphNode ->
  Map.Map NodeKey ModuleGraphNode
mergeModuleGraphNodes new oldMap = merged
  where
    !merged = Map.unionWith mergeNodes oldMap newMap

    mergeNodes = \cases
      old@(ModuleNode _oldDeps _oldSumm) (ModuleNode _newDeps _newSumm) -> old
      _ newNode -> newNode

    newMap = Map.fromList $ [(mkNodeKey n, n) | n <- new]

mergeModuleGraph ::
  [(NodeKey, (Int, ModuleGraphNode))] ->
  EModuleGraph ->
  EModuleGraph
mergeModuleGraph kinodes egr =
  MG.extendReachIndex $ foldr MG.extendMG' egr kinodes

storeModuleGraphNodes :: [ModuleGraphNode] -> MakeState -> MakeState
storeModuleGraphNodes new state =
  state {
    moduleGraphState = egr',
    moduleGraphNodes = merged
  }
  where
    !merged = mergeModuleGraphNodes new state.moduleGraphNodes
    egr = state.moduleGraphState
    kinMap = egr.keyIndexNodeMap
    egr' = egr { keyIndexNodeMap = kinMap }

-- | Derive 'moduleGraph' from the node index.
--
-- This is @O(size of the index)@, so when a batch of units is restored it must be called once for the batch rather than
-- once per unit.
rebuildModuleGraph :: Bool -> MakeState -> MakeState
rebuildModuleGraph use_incr !state
  | use_incr = rebuildModuleGraphIncr state
  | otherwise = rebuildModuleGraphNonIncr state

rebuildModuleGraphNonIncr :: MakeState -> MakeState
rebuildModuleGraphNonIncr !state =
  let old_egr = state.moduleGraphState
      new_gr = GHC.MG.mkModuleGraph (Map.elems state.moduleGraphNodes)
      new_egr = old_egr {
        moduleGraph = new_gr
      }
   in state {moduleGraphState = new_egr}

rebuildModuleGraphIncr :: MakeState -> MakeState
rebuildModuleGraphIncr !state =
  let old_egr = state.moduleGraphState
      KIN old_kmap old_inodes old_kss old_i2k old_reach = state.moduleGraphState.keyIndexNodeMap
      old_keys = Set.fromList (Map.keys old_kmap)
      old_n = Set.size old_keys
      all_nodes = state.moduleGraphNodes
      all_keys = Set.fromList (Map.keys all_nodes)
      all_n = Set.size all_keys
      delta_keys = all_keys `Set.difference` old_keys
      delta_kmap_list = zip (Set.toList delta_keys) [old_n + 1 .. all_n]
      delta_kmap = Map.fromList delta_kmap_list
      all_kmap = old_kmap `Map.union` delta_kmap
      delta_knodes = filter (\(k, _) -> k `Set.member` delta_keys) (Map.toList all_nodes)

      delta_kinodes_list :: [(NodeKey, (Int, ModuleGraphNode))]
      delta_kinodes_list = do
        (k, node) <- delta_knodes
        i <- maybeToList (Map.lookup k all_kmap)
        pure (k, (i, node))

      all_inodes = IM.union (IM.fromList (fmap snd delta_kinodes_list)) old_inodes

      delta_i2k = IM.fromList [ (i,k) | (k, (i, _)) <- delta_kinodes_list ]
      all_i2k = IM.union delta_i2k old_i2k

      -- BE CAREFUL old_kss and old_reach
      newKIN0 = KIN all_kmap all_inodes old_kss all_i2k old_reach
      new_egr1 = mergeModuleGraph delta_kinodes_list (old_egr {keyIndexNodeMap = newKIN0})
      newKIN1 = keyIndexNodeMap new_egr1
      new_egr = new_egr1 { keyIndexNodeMap = newKIN1 }
   in state {
     moduleGraphState = new_egr
   }

-- | Merge the given module graph into the cached graph and derive 'moduleGraph' immediately.
storeModuleGraph :: Bool -> ModuleGraph -> MakeState -> MakeState
storeModuleGraph use_incr new =
  rebuildModuleGraph use_incr . storeModuleGraphNodes (mgModSummaries' new)

-- | Extract the unit env of the currently active unit and store it in the cache.
-- This is used by the make mode worker after the metadata step has initialized the new unit, and when a unit is
-- restored from the Buck cache.
--
-- The native libraries the unit's flags name with @-L@ and @-l@ are recorded for
-- 'Internal.State.Linkables.ensureLibraries', which loads them when a module of the unit is first linked for
-- Template Haskell, and removed from the flags that are stored. GHC's loader initialises once per 'Interp', in
-- whichever request first runs a splice, and at that point loads the libraries named by the flags of every home unit
-- in the session. On a remote executor each request runs in an execution root that holds the libraries of the unit
-- it compiles and of that unit's dependencies, so a unit registered or restored by an earlier request for an
-- unrelated unit would make that initialisation fail with
-- @user specified .o/.so/.DLL could not be loaded@ for a library the root never received.
-- The live session keeps the flags as parsed, so the request that registered the unit still links it the usual way.
--
-- TODO: this ad hoc extraction of extra library dependency should be replaced by proper specification
-- from the build system rules and recorded in a file, preferrably to buildplan file.
insertUnitEnv :: HscEnv -> MakeState -> MakeState
insertUnitEnv hsc_env state =
  state {
    hug = update state.hug,
    extraLib = requestLibraries current ue.homeUnitEnv_dflags state.extraLib,
    unitGenerations = Map.insert current state.nextGeneration state.unitGenerations,
    nextGeneration = state.nextGeneration + 1
  }
  where
    ue = unitEnv_lookup current hsc_env.hsc_unit_env.ue_home_unit_graph
    current = hsc_env.hsc_unit_env.ue_current_unit
    update = unitEnv_insert current (withoutLinkInputs ue)

-- | Record the library search paths and libraries a unit's flags name, for 'ensureLibraries'.
requestLibraries :: UnitId -> DynFlags -> LibLoadState -> LibLoadState
requestLibraries unit dflags libs =
  libs {requested = Map.insert unit (libraryPaths dflags, [lib | Option ('-' : 'l' : lib) <- ldInputs dflags]) libs.requested}

-- | A home unit env whose flags name no library search paths and no link inputs; see 'insertUnitEnv'.
withoutLinkInputs :: HomeUnitEnv -> HomeUnitEnv
withoutLinkInputs ue =
  ue {homeUnitEnv_dflags = ue.homeUnitEnv_dflags {libraryPaths = [], ldInputs = []}}

-- | What a request restored and adopted, for writing its work back and releasing its interpreter when it ends.
data Request =
  Request {
    token :: Int,
    -- | The unit the request compiles in, and that unit's generation when the request restored it.
    active :: UnitId,
    generation :: Maybe Word64,
    -- | The active unit's home package table as restored. The request's compile runs on a copy, so what differs from
    -- this at the end is the request's own work.
    snapshot :: DModuleNameEnv HomeModInfo,
    claim :: Map.Map HomeModuleKey CodeVersion,
    interp :: Maybe (Int, Interp)
  }

-- | See 'CodeVersion'.
codeVersion :: ModIface -> CodeVersion
codeVersion iface =
  fingerprintFingerprints hashes
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    hashes = [mi_src_hash iface, mi_mod_hash iface]
#else
    hashes = [mi_src_hash iface, mi_mod_hash (mi_final_exts iface), mi_flag_hash (mi_final_exts iface)]
#endif

homeModuleKey :: ModIface -> HomeModuleKey
homeModuleKey iface = (toUnitId (moduleUnit (mi_module iface)), moduleName (mi_module iface))

-- | Recorded for a module found linked into an interpreter that no request claimed, so no claim ever agrees with it.
unknownVersion :: CodeVersion
unknownVersion = fingerprintString "ghc-worker: linked by no claim"

-- | The interpreters kept for the requests to come, beyond those in use. Each holds the bytecode it linked; two let
-- two commits that alternate keep theirs, and the rest absorb a third build without evicting either.
maxIdleInterps :: Int
maxIdleInterps = 4

-- | Turn a restored session into a request of its own, under the state lock, once setup has restored what the request
-- needs. Every home package table in the session becomes a copy, so the request's compile reads what it restored and
-- nothing a concurrent request loads into the stored tables meanwhile, and its own results stay out of them until
-- 'commitRequest'. The session then runs its splices in a kept interpreter that agrees with its claim, or in the fresh
-- one its flags gave it, which is kept from then on.
beginRequest :: Logger -> Map.Map HomeModuleKey CodeVersion -> HscEnv -> MakeState -> IO (MakeState, Request, HscEnv)
beginRequest logger claim hsc_env state = do
  snapshot <- maybe (pure emptyUDFM) (readIORef . hptInternalTableRef . (.homeUnitEnv_hpt)) (unitEnv_lookup_maybe active hug)
  private <- UnitEnvGraph <$> traverse privateHpt graph
  let (interps, joined, fresh) = joinInterp token claim hsc_env.hsc_interp state.interps
  when fresh $ for_ joined \ (key, _) ->
    logger.info ("ghc-worker: splices run in interpreter " ++ show key ++ ", since no kept one agrees with this request's " ++ show (Map.size claim) ++ " dependencies")
  let request = Request {token, active, generation = Map.lookup active state.unitGenerations, snapshot, claim, interp = joined}
      session = hsc_env {
        hsc_unit_env = hsc_env.hsc_unit_env {ue_home_unit_graph = private},
        hsc_interp = maybe hsc_env.hsc_interp (Just . snd) joined
      }
  pure (state {interps, nextRequest = token + 1}, request, session)
  where
    token = state.nextRequest
    active = hsc_env.hsc_unit_env.ue_current_unit
    hug = hsc_env.hsc_unit_env.ue_home_unit_graph
    UnitEnvGraph graph = hug

    privateHpt hue = do
      table <- newIORef =<< readIORef (hptInternalTableRef hue.homeUnitEnv_hpt)
      pure hue {homeUnitEnv_hpt = hptInternalTableFromRef table}

-- | The kept interpreter most recently joined whose record and running claims agree with the claim, or, when there is
-- none, the session's own, added to the pool; the flag says the latter.
joinInterp :: Int -> Map.Map HomeModuleKey CodeVersion -> Maybe Interp -> InterpPool -> (InterpPool, Maybe (Int, Interp), Bool)
joinInterp token claim own pool =
  case filter agrees (sortOn (Down . (.lastUsed)) pool.interps) of
    si : _ ->
      (pool {interps = [if e.key == si.key then join e else e | e <- pool.interps]}, Just (si.key, si.interp), False)
    [] ->
      case own of
        Nothing -> (pool, Nothing, False)
        Just i ->
          let new = SharedInterp {key = pool.nextKey, interp = i, linked = Map.empty, claims = IM.singleton token claim, lastUsed = token}
          in (pruneInterps pool {interps = new : pool.interps, nextKey = pool.nextKey + 1}, Just (new.key, i), True)
  where
    join e = e {claims = IM.insert token claim e.claims, lastUsed = token}

    agrees si = and [same v (Map.lookup k si.linked) && all (same v . Map.lookup k) (IM.elems si.claims) | (k, v) <- Map.toList claim]

    same v = maybe True (== v)

-- | Drop the least recently used idle interpreters beyond 'maxIdleInterps'. One in use stays: its requests hold it, and
-- 'leaveInterp' finds it gone only if it was dropped, which it never is while claimed.
pruneInterps :: InterpPool -> InterpPool
pruneInterps pool =
  pool {interps = [e | e <- pool.interps, e.key `notElem` dropped]}
  where
    idle = sortOn (.lastUsed) [e | e <- pool.interps, IM.null e.claims]
    dropped = (.key) <$> take (length idle - maxIdleInterps) idle

-- | The home modules linked into an interpreter. Read without the state lock: a request's splice holds the loader's
-- lock while it links, and the linker's hook takes the state lock for native libraries.
loadedModules :: Interp -> IO [HomeModuleKey]
loadedModules interp =
  readMVar (loader_state (interpLoader interp)) <&> \case
    Nothing -> []
    Just ls -> [(toUnitId (moduleUnit m), moduleName m) | m <- moduleEnvKeys (bcos_loaded ls) ++ moduleEnvKeys (objs_loaded ls)]

-- | Release a request's claim on its interpreter and record the code versions of the modules now linked into it: the
-- request's own version for what it claimed, nothing yet for what another running request claimed (that one records
-- it), and 'unknownVersion' for anything else.
leaveInterp :: Request -> [HomeModuleKey] -> InterpPool -> InterpPool
leaveInterp req loaded pool =
  case req.interp of
    Nothing -> pool
    Just (key, _) -> pruneInterps pool {interps = [if e.key == key then leave e else e | e <- pool.interps]}
  where
    leave e =
      e {claims = others, linked = foldl' record e.linked loaded}
      where
        others = IM.delete req.token e.claims

        record acc m
          | Map.member m acc = acc
          | Just v <- Map.lookup m req.claim = Map.insert m v acc
          | any (Map.member m) (IM.elems others) = acc
          | otherwise = Map.insert m unknownVersion acc

-- | Write a request's work on its unit back to the stored home package table, under the state lock: the modules its
-- compile added or replaced in its copy, and those it gave bytecode. Only while the unit still has the generation the
-- request restored: a unit another request evicted or restored anew meanwhile was built from other flags or another
-- module set, and this request's modules do not belong in it.
commitRequest :: Logger -> Request -> HscEnv -> MakeState -> IO ()
commitRequest logger req hsc_env state = do
  logMemStats "store make state" logger
  case (req.generation, Map.lookup req.active state.unitGenerations, unitEnv_lookup_maybe req.active state.hug, unitEnv_lookup_maybe req.active hsc_env.hsc_unit_env.ue_home_unit_graph) of
    (Just restored, Just current, Just stored, Just private)
      | restored == current -> do
        table <- readIORef (hptInternalTableRef private.homeUnitEnv_hpt)
        for_ (eltsUDFM table) \ hmi ->
          when (changed hmi) (addHomeModInfoToHpt hmi stored.homeUnitEnv_hpt)
    (Just _, _, _, _) ->
      logger.info ("ghc-worker: keep the stored " ++ showPprUnsafe req.active ++ ": another request replaced it while this one ran")
    _ -> pure ()
  where
    changed hmi =
      case lookupUDFM req.snapshot (moduleName (mi_module hmi.hm_iface)) of
        Nothing -> True
        Just old -> codeVersion old.hm_iface /= codeVersion hmi.hm_iface || (hasCode hmi && not (hasCode old))

    hasCode hmi = isJust (homeModInfoByteCode hmi) || isJust (homeModInfoObject hmi)
