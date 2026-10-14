{-# LANGUAGE CPP, OverloadedLists, PatternSynonyms #-}

module Internal.Cache.Hpt where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), get, put)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Map.Strict qualified as M (Map, delete, insert, lookup)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set (Set, empty, insert, member, singleton, toList)
import Data.Time (getCurrentTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC (DynFlags, GhcException (..), IsBootInterface (..), ModIface, ModIface_ (..), ModLocation (..), Module, ModuleName, mkModule, mkModuleName, moduleName, moduleNameString)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Word (Word32)
import GHC.Data.Bag (emptyBag)
import qualified GHC.Utils.Binary as Binary
import GHC.Utils.Panic (tryMost)
import Internal.AbiHash (parseSidecar, showAbiHash)
import GHC.Fingerprint (Fingerprint, fingerprint0, fingerprintFingerprints, fingerprintString, getFileHash)
import GHC.Data.Maybe (MaybeErr (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hscSetActiveUnitId, hsc_HPT)
import GHC.Driver.Main (initModDetails)
import GHC.Driver.Make (ModNodeKeyWithUid (..))
import GHC.Driver.Session (dynHiSuf_, dynamicNow, hiDir, hiSuf_, targetProfile)
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Iface.Errors.Ppr (readInterfaceErrorDiagnostic)
import GHC.Iface.Errors.Types (ReadInterfaceError (..))
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (mkOccEnv)
import GHC.Types.Name.Reader (GlobalRdrEltX (..), Parent (NoParent))
import GHC.Unit (Definite (..), GenUnit (..), GenWithIsBoot (..), UnitId, moduleUnitId)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..), homeModInfoByteCode)
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Module (moduleNameSlashes)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey (..))
import GHC.Unit.Module.Location (addBootSuffix, pattern ModLocation)
import GHC.Unit.Module.Deps (Dependencies (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Module.ModIface (IfaceTopEnv (..), mi_module, mi_src_hash, set_mi_top_env)
import Internal.State (updateMakeState)
import Internal.State.Make (evictUnit)
import qualified Internal.State.Make as Make
import GHC.Unit.Module.WholeCoreBindings (WholeCoreBindings (..))
import GHC.Utils.Misc (modificationTimeIfExists)
import GHC.Utils.Outputable (ppr, showPprUnsafe, ($+$))
import GHC.Utils.Panic (throwGhcExceptionIO, tryMost)
import Internal.Cache.Metadata (Verdict (..), decodeJsonBuildPlan, loadCachedHomeUnit, loadCachedDepUnits, readParseGHCArgs, validateStoredUnit)
import Internal.Compat.FixedNodes (pattern CompileNode, pattern FixedNode, deps)
import Internal.Compat.GHC914 (edgeTarget, setExtraDecls)
import Internal.Log (logTimed)
import Prelude hiding (log)
import System.Directory (getFileSize)
import System.FilePath ((<.>), (</>))
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import Types.BuckArgs (IsInterpreted (Compiled, Interpreted), decodeJsonArg)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedDep (..), CachedDeps (..), CachedUnit (..), JsonFs (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (make))
import Types.State.Make (bcoLoadState, depUnitFlags, unitPlans)

#if defined(MWB)

import GHC.Unit.Module.ModIface (mi_foreign)

#elif MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Types.Avail (sortAvails)
import GHC.Types.Name.Reader (globalRdrEnvElts, gresToAvailInfo)
import GHC.Unit.Module.ModIface (mi_sc_extra_decls, mi_sc_foreign)

#endif

#if defined(MWB)

import GHC.Driver.Main(compileWholeCoreBindings)

-- This is basically initWholeCoreBindings, but strict version and
-- does not add empty HMI to HPT.
loadWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  Linkable ->
  IO Linkable
loadWholeCoreBindings hsc_env _iface details (Linkable utc_time this_mod uls) =
  Linkable utc_time this_mod <$> mapM go uls
  where
    go = \case
      CoreBindings wcb -> do
        -- we only add byte code objects.
        (bco, _fos) <- compileWholeCoreBindings hsc_env type_env wcb
        pure (BCOs bco)
      l -> pure l
    type_env = md_types details

#else

import GHC.Driver.Main(initWholeCoreBindings)

loadWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  Linkable ->
  IO Linkable
loadWholeCoreBindings = initWholeCoreBindings

#endif


#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

coreBindings :: ModIface -> ModLocation -> Maybe WholeCoreBindings
coreBindings iface wcb_mod_location =
  mi_simplified_core iface <&> \ sc ->
    WholeCoreBindings {wcb_mod_location, wcb_bindings = mi_sc_extra_decls sc, wcb_foreign = mi_sc_foreign sc, wcb_module = mi_module iface, ..}

#elif defined(MWB)

coreBindings :: ModIface -> ModLocation -> Maybe WholeCoreBindings
coreBindings iface wcb_mod_location =
  mi_extra_decls iface <&> \ wcb_bindings ->
    WholeCoreBindings {wcb_mod_location, wcb_foreign = mi_foreign iface, wcb_module = mi_module iface, ..}

#endif


loadCachedByteCodeFrom :: HscEnv -> ModLocation -> ModIface -> ModDetails -> IO (Maybe Linkable)
loadCachedByteCodeFrom hsc_env location iface details =
  for (coreBindings iface location) \ wcb -> do
    linkable <- bcoLinkable [CoreBindings wcb]
    loadWholeCoreBindings hsc_env iface details linkable
   where
    bcoLinkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file location)
      time <- maybe getCurrentTime pure if_time
      return $! Linkable time (mi_module iface) parts

-- | Load bytecode from an interface.
-- Used only for modules missing from the current target's HPT when restoring the Buck cache after restarting a build.
--
-- The missing fields in @ModLocation@ aren't vital for the bytecode's purpose, but it wouldn't hurt to add them
-- eventually.
-- For example, the source file is used to add debug info and find foreign export stubs.
loadCachedByteCode :: HscEnv -> FilePath -> ModIface -> ModDetails -> IO (Maybe Linkable)
loadCachedByteCode hsc_env ifaceFile iface details =
  loadCachedByteCodeFrom hsc_env location iface details
   where
    location =
      ModLocation {
        ml_hs_file = Nothing,
        ml_hi_file = ifaceFile,
        ml_dyn_hi_file = ifaceFile,
        ml_obj_file = error "loadCachedByteCode",
        ml_dyn_obj_file = error "loadCachedByteCode",
        ml_hie_file = error "loadCachedByteCode"
      }

-- | module loading state
data ModuleLoadState =
  Loaded
  |
  Waiting (MVar ())
  |
  RequestHi (MVar ())
  |
  RequestBCO (MVar ()) HomeModInfo

-- | Why an in-memory 'HomeModInfo' may no longer stand in for the interface on disk, or 'Nothing' when it may. Fail
-- closed: a missing @.hash@ sidecar, or one without a source hash, counts as stale. Both hashes come from that
-- sidecar, which the compile wrote with @--abi-out@. The source hash catches a source edit whose ABI did not move, a
-- value-only change; the ABI hash catches an ABI change under an unchanged source, such as a CPP-gated export toggled
-- by a unit flag. Reading the source hash here rather than out of the interface's header keeps this check clear of the
-- interface's binary layout, which moves between compiler versions.
interfaceStale :: HscEnv -> HomeModInfo -> OsPath -> IO (Maybe String)
interfaceStale hsc_env hmi ifaceFile =
  readSidecar (fromOsPath ifaceFile ++ ".hash") <&> \case
    Nothing -> Just "no .hash beside the interface"
    Just text -> case parseSidecar text of
      Nothing -> Just ".hash predates the source hash"
      Just (abi, src)
        | norm src /= norm (show (mi_src_hash hmi.hm_iface)) -> Just "source hash changed"
        | norm abi /= norm (showAbiHash hsc_env hmi.hm_iface) -> Just "ABI hash changed"
        | otherwise -> Nothing
  where
    readSidecar p = tryMost (readFile p >>= \ str -> length str `seq` pure str) <&> either (const Nothing) Just
    norm = filter (not . isSpace)

-- | The home modules this request has reloaded so far, by unit.
type Reloaded = Set.Set (UnitId, ModuleName)

-- | A kept module one of whose direct home dependencies this request has already reloaded, or 'Nothing'. Its own
-- hashes can still match the interface on disk, since an edit low in the graph moves neither a dependent's source nor,
-- unless it changes what the dependent exports, its ABI; but the kept 'HomeModInfo' was typechecked against the
-- dependency's previous version, and on bzlw (2026-10-13/14, GHC-83249) a later spec splicing in that module's
-- Template Haskell instances failed to find a type the module itself declares. Dependencies come before dependents in
-- a request ('depsFromModuleGraph' is postorder), so a dependency's reload is known by the time its dependent is
-- checked, and dropping the dependent adds it to the set in turn.
dependencyReloaded :: Reloaded -> HomeModInfo -> Maybe String
dependencyReloaded reloaded hmi =
  case [ (u, n) | (u, GWIB n _) <- Set.toList (dep_direct_mods (mi_deps hmi.hm_iface)), Set.member (u, n) reloaded ] of
    (_, n) : _ -> Just ("its dependency " ++ moduleNameString n ++ " was reloaded")
    [] -> Nothing

prepareHmiLoader ::
  Logger ->
  HscEnv ->
  Reloaded ->
  ModuleName ->
  OsPath ->
  StateT WorkerState IO (ModuleLoadState, Bool)
prepareHmiLoader logger hsc_env reloaded name ifaceFile = do
  existing <- liftIO (lookupHpt hpt name)
  case existing of
    Just hmi ->
      liftIO (interfaceStale hsc_env hmi ifaceFile) <&> maybe (dependencyReloaded reloaded hmi) Just >>= \case
        Nothing
          | isJust (homeModInfoByteCode hmi) -> pure (Loaded, False)
          | otherwise -> (, False) <$> updateBcoState
        Just reason -> do
          let modu = mi_module hmi.hm_iface
          liftIO $ logger.info ("ghc-worker: reload " ++ showPprUnsafe modu ++ ": " ++ reason)
          s <- get
          -- An interpreter that linked the old code keeps it; a request claiming the new version no longer agrees
          -- with that interpreter and gets another, see 'Types.State.Make.SharedInterp'.
          put s {make = s.make {bcoLoadState = M.delete key s.make.bcoLoadState}}
          (, True) <$> updateBcoState
    Nothing -> (, False) <$> updateBcoState
  where
    hpt = hsc_HPT hsc_env

    key = (hscActiveUnitId hsc_env, name)

    updateBcoState = do
      new_lock <- liftIO newEmptyMVar
      s <- get
      let make = s.make
          m = make.bcoLoadState
          mlock = M.lookup key m
      case mlock of
        Nothing -> do
          let m' = M.insert key new_lock m
              make' = make {bcoLoadState = m'}
          put s {make = make'}
          pure (RequestHi new_lock)
        Just lock -> pure (Waiting lock)

-- | If the given module name is missing from the HPT, load the given interface from disk and store it in the module's
-- 'HomeModInfo'.
--
-- This only happens when the module is depended upon downstream for the first time after restarting the worker with a
-- partial build.
--
-- Maybe this could reuse some stuff in @hscRecompStatus@?
loadCachedDep ::
  Logger ->
  FeatureFlags ->
  IsInterpreted ->
  HscEnv ->
  ModuleName ->
  OsPath ->
  ModuleLoadState ->
  IO ModuleLoadState
loadCachedDep log features interp hsc_env name ifaceFile mod_load_state =
  case mod_load_state of
    Loaded -> pure Loaded
    Waiting lock -> readMVar lock >> pure Loaded
    RequestHi lock -> loadHmiOnlyInterface >>= \ hmi -> pure (RequestBCO lock hmi)
    RequestBCO lock hmi -> loadHmiFull hmi >> putMVar lock () >> pure Loaded
  where
    loadHmiOnlyInterface = do
      logTimed log ("Loading HPT module from cache (interface): " ++ fromOsPath ifaceFile) do
        hm_iface <- loadIface
        !hm_details <- initModDetails hsc_env hm_iface
        let hmi = HomeModInfo {
          hm_iface,
          hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode = Nothing},
          hm_details
        }
        addHomeModInfoToHpt hmi hpt
        pure hmi

    loadHmiFull HomeModInfo {hm_iface, hm_details} = do
      logTimed log ("Loading HPT module from cache (BCO): " ++ fromOsPath ifaceFile) do
        homeMod_bytecode <-
          if lazyByteCode
          then pure Nothing
          else loadCachedByteCode hsc_env (fromOsPath ifaceFile) hm_iface hm_details
        let hm_iface' = (if lazyByteCode then id else setExtraDecls Nothing) hm_iface
        let hmi' = HomeModInfo {
          hm_iface = hm_iface',
          hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode},
          hm_details
        }
        addHomeModInfoToHpt hmi' hpt

    -- Deferring bytecode needs the linkables support that -flinkables compiles in, so a build without it loads the
    -- cached bytecode whatever the flag says. mwb builds with -flinkables; the repository's default build does not,
    -- and its tests reload modules for Template Haskell through this path.
#if defined(LINKABLES)
    lazyByteCode = features.lazyByteCode
#else
    lazyByteCode = False
#endif

    -- @readIface@ needs the dflags only for platform/ways, so we don't need the unit dflags
    loadIface =
      ifaceResult =<< readIface' (hsc_dflags hsc_env) (hsc_NC hsc_env) (toModule name) (fromOsPath ifaceFile)

    -- NOTE: We use this custom version of readIface to ignore the hi way (i.e. CheckHiWay -> IgnoreHiWay)
    readIface' dflags name_cache wanted_mod file_path = do
      let profile = targetProfile dflags
      res <- tryMost $ readBinIface profile name_cache IgnoreHiWay QuietBinIFace file_path
      case res of
        Right iface
          -- NB: This check is NOT just a sanity check, it is
          -- critical for correctness of recompilation checking
          -- (it lets us tell when -this-unit-id has changed.)

          -- NOTE: mi_top_env is synthesized in order to make the symbols
          -- from the loaded interfaces be avaiable for the evaluated
          -- expression in the interpreter session.
          | wanted_mod == actual_mod && interp == Interpreted ->
              let es = mi_exports iface
                  convert (Avail n) = Just (nameOccName n, [GRE {gre_name = n, gre_par = NoParent, gre_lcl = True, gre_imp = emptyBag, gre_info = ()}])
                  convert (AvailTC _ _) = Nothing

                  exports = mkOccEnv (mapMaybe convert es)
                  imports = []
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
                  -- Unclear if this is equivalent.
                  rdrs = IfaceTopEnv (sortAvails (gresToAvailInfo (globalRdrEnvElts exports))) imports
#else
                  rdrs = Just (IfaceTopEnv exports imports)
#endif
               in return (Succeeded (set_mi_top_env rdrs iface))
          | wanted_mod == actual_mod && interp == Compiled -> return (Succeeded iface)
          | otherwise     -> return (Failed err)
          where
            actual_mod = mi_module iface
            err = HiModuleNameMismatchWarn file_path wanted_mod actual_mod
        Left exn -> return (Failed (ExceptionOccurred file_path exn))

    ifaceResult = \case
      Succeeded i ->
        pure i
      Failed err ->
        let msg = ppr name $+$ readInterfaceErrorDiagnostic err
        in throwGhcExceptionIO (PprProgramError "Loading cached interface failed" msg)

    toModule = mkModule (RealUnit (Definite uid))

    uid = hscActiveUnitId hsc_env

    hpt = hsc_HPT hsc_env

hasUnit :: UnitId -> HscEnv -> Bool
hasUnit uid hsc_env =
  isJust $ unitEnv_lookup_maybe uid hsc_env.hsc_unit_env.ue_home_unit_graph

-- | The canonical path of a home unit module's interface file, derived from the unit's flags and the module name:
--
-- > hidir </> module name with dots replaced by slashes <.> hisuf
--
-- Example, with @-hidir out -hisuf dyn_hi@:
--
-- > Data.Vector      -> out/Data/Vector.dyn_hi
-- > Data.Vector-boot -> out/Data/Vector.dyn_hi-boot
--
-- This path is a contract between the worker and callers.
canonicalInterfacePath :: DynFlags -> ModuleName -> Maybe OsPath
canonicalInterfacePath dflags name =
  hiDir dflags <&> \ dir ->
    mkBoot (toOsPath (dir </> plainName <.> hiSuf))
  where
    hiSuf
      | dynamicNow dflags = dynHiSuf_ dflags
      | otherwise = hiSuf_ dflags

    (plainName, mkBoot) = case stripSuffix "-boot" (moduleNameSlashes name) of
      Just plain -> (plain, addBootSuffix)
      Nothing -> (moduleNameSlashes name, id)

    stripSuffix suf str
      | suf `isSuffixOf` str = Just (take (length str - length suf) str)
      | otherwise = Nothing

-- | Compute the transitive dependency closure of a given module from the module graph, in dependency postorder.
depsFromModuleGraph :: M.Map NodeKey ModuleGraphNode -> Module -> CachedDeps
depsFromModuleGraph nodes target =
  CachedDeps (reverse (snd (children (Set.singleton targetKey, []) targetKey)))
  where
    children acc key =
      case M.lookup key nodes of
        Just CompileNode {deps} -> foldl' visit acc (edgeTarget <$> deps)
        Just FixedNode {deps} -> foldl' visit acc (edgeTarget <$> deps)
        _ -> acc

    visit (seen, deps) key
      | Set.member key seen = (seen, deps)
      | otherwise =
          let (seen', deps') = children (Set.insert key seen, deps) key
          in (seen', maybe deps' (: deps') (cachedDep key))

    cachedDep = \case
      NodeKey_Module (ModNodeKeyWithUid (GWIB name isBoot) uid) ->
        Just CachedDep {name = JsonFs (bootName isBoot name), package = JsonFs uid}
      _ ->
        Nothing

    bootName IsBoot name = mkModuleName (moduleNameString name ++ "-boot")
    bootName NotBoot name = name

    targetKey = NodeKey_Module (ModNodeKeyWithUid (GWIB (moduleName target) NotBoot) (moduleUnitId target))

-- | Load all dependencies of the current module from the Buck cache into the HPT if they don't exist.
--
-- When the make worker is killed by Buck at the end of a build, and the user subsequently changes some code and starts
-- a new build, the state (the current HPT) is initially empty, since Buck immediately tries to compile the changed
-- module, assuming its deps to be available to the compiler.
loadCachedDeps ::
  Logger ->
  FeatureFlags ->
  IsInterpreted ->
  (WorkerState, HscEnv) ->
  CachedDeps ->
  IO (WorkerState, HscEnv)
loadCachedDeps log features interp (state0, hsc_env0) (CachedDeps deps) =
  logTimed log "Loading cached deps" do
    (state1, hsc_env1, _) <- foldM loadDepUnit (state0, hsc_env0, Set.empty) byUnit
    pure (state1, hscSetActiveUnitId (hscActiveUnitId hsc_env0) hsc_env1)
  where
    -- If the unit isn't present in the unit env, it wasn't built by a worker, since it would have been loaded in the
    -- metadata restoration step.
    loadDepUnit (state, hsc_env, reloaded) mods@(CachedDep {package = JsonFs uid} :| _) =
      if hasUnit uid hsc_env
      then do
        let hsc_env' = hscSetActiveUnitId uid hsc_env
        (state', reloaded') <- loadActiveUnit hsc_env' state reloaded (toList mods)
        pure (state', hsc_env', reloaded')
      else pure (state, hsc_env, reloaded)

    loadActiveUnit :: HscEnv -> WorkerState -> Reloaded -> [CachedDep] -> IO (WorkerState, Reloaded)
    loadActiveUnit hsc_env state reloaded0 mods =
      fmap swap $ flip runStateT state do
        (mod_plans, reloaded') <- foldM (\ (acc, reloaded) dep -> do
            (plan, didReload) <- prepareDep hsc_env reloaded dep
            let (name, _, _) = plan
            pure (plan : acc, if didReload then Set.insert (hscActiveUnitId hsc_env, name) reloaded else reloaded)
          ) ([], reloaded0) mods
        liftIO $ for_ (reverse mod_plans) \ (name, iface, mod_load_state) -> do
          mod_load_state' <- loadCachedDep log features interp hsc_env name iface mod_load_state
          loadCachedDep log features interp hsc_env name iface mod_load_state'
        pure reloaded'

    prepareDep hsc_env reloaded CachedDep {name = JsonFs name, package = JsonFs uid} = do
      iface <- maybe (missingHiDir uid name) pure (canonicalInterfacePath (hsc_dflags hsc_env) name)
      (mod_load_state, didReload) <- prepareHmiLoader log hsc_env reloaded name iface
      pure ((name, iface, mod_load_state), didReload)

    missingHiDir uid name =
      liftIO $ throwGhcExceptionIO $
        PprProgramError "loadCachedDeps: unit has no -hidir set, cannot derive interface path" $
          ppr uid $+$ ppr name

    byUnit = groupBy (on (==) (.package)) deps

loadHomeUnit ::
  Logger ->
  DynFlags ->
  FeatureFlags ->
  UnitId ->
  (WorkerState, HscEnv) ->
  OsPath ->
  IO (WorkerState, HscEnv)
loadHomeUnit log dflags0 features unit (state0, hsc_env0) path = do
  cachedUnit@CachedUnit {unit_args} <- decodeJsonArg "--home-unit" path
  plan <- unitFlagsFingerprint unit_args
  (depFlags, depCache) <- dependencyUnitFlags state0.make.depUnitFlags cachedUnit.dep_units
  -- A dependency unit rebuilt by another server is still here under the same
  -- id, and nothing on a compile request's path reads its plan: a present home
  -- unit returns before loadCachedDepUnits, and a flag that changes a value
  -- rather than an export moves neither the interface's source hash nor its
  -- ABI, so the loader's per-module check cannot see it either. Its args file
  -- can.
  let movedDeps = [u | (u, fp) <- depFlags, maybe False (/= fp) (M.lookup u state0.make.unitPlans)]
  for_ movedDeps \ u ->
    log.info ("ghc-worker: evict dep unit " ++ showPprUnsafe u ++ ": its flags have changed")
  let state0D = foldr (\ u -> updateMakeState (evictUnit features.useIncrModGraph u)) state0 movedDeps
      recordDeps = updateMakeState \ make ->
        make {
          unitPlans = foldr (\ (u, fp) -> M.insert u fp) make.unitPlans depFlags,
          depUnitFlags = depCache
        }
  let recorded = M.lookup unit state0D.make.unitPlans
      stale = maybe False (/= plan) recorded
  -- The args bytes above are the fast signal; a kept unit whose bytes match is still checked against GHC's own flag
  -- fingerprint and the plan's module set, the record it was stored with, which reaches a unit the bytes cannot tell
  -- apart. That check logs its own reason when it evicts.
  (fingerprintStale, state0F) <-
    if hasUnit unit hsc_env0 && not stale
    then validateStoredUnit log features hsc_env0 state0D.make unit path <&> \case
      Valid Nothing -> (False, state0D)
      Valid (Just fp) -> (False, updateMakeState (Make.storeUnitFingerprint unit fp) state0D)
      Stale _ -> (True, state0D)
    else pure (False, state0D)
  if hasUnit unit hsc_env0 && not stale && not fingerprintStale && null movedDeps
  -- A unit defined by a metadata request is already in the graph when the first
  -- compile arrives, so this is where its flags get recorded. Without that there
  -- is nothing for a later request to differ from.
  then pure (recordDeps (recordUnitFlags unit plan state0F), hsc_env0)
  else do
    -- A unit restored again below starts from an empty module table ('loadCachedHomeUnit' inserts a fresh home unit
    -- env), so it is evicted first whenever it is present, a moved dependency unit included: eviction also drops the
    -- unit's module load state, and a load state left behind says "loaded" for a module the new table lacks, so the
    -- next compile that imports it fails with GHC-58427 ("attempting to use module ... which is not loaded"). The
    -- kept modules were compiled against the evicted dependency's interfaces, so none of them could be kept anyway.
    state0' <-
      if not (hasUnit unit hsc_env0)
      then pure state0F
      else do
        when stale $ log.info ("ghc-worker: evict unit " ++ showPprUnsafe unit ++ ": its flags have changed")
        when (not stale && not fingerprintStale) $
          log.info ("ghc-worker: evict unit " ++ showPprUnsafe unit ++ ": a dependency unit's flags have changed")
        pure (updateMakeState (evictUnit features.useIncrModGraph unit) state0F)
    (state1, hsc_env1) <- fmap (fromMaybe (state0', hsc_env0)) $ for cachedUnit.dep_units \ file -> do
      deps <- decodeJsonArg "--home-unit" file
      loadCachedDepUnits log dflags0 deps features (state0', hsc_env0)
    dflags <- maybe (pure dflags0) (readParseGHCArgs features.flagParser hsc_env1 dflags0) unit_args
    (state2, hsc_env2) <- logTimed log "Loading cached home unit" $ fmap swap do
      runStateT (loadCachedHomeUnit log features.fixedNodesCache features.useIncrModGraph hsc_env1 unit path (cachedUnit, dflags)) state1
    pure (recordDeps (recordUnitFlags unit plan state2), hsc_env2)

recordUnitFlags :: UnitId -> Fingerprint -> WorkerState -> WorkerState
recordUnitFlags unit plan =
  updateMakeState \ make -> make {unitPlans = M.insert unit plan make.unitPlans}

-- | The flags a unit was built with, as the bytes of the args file its plan
-- names. A compile request names the plan, never the flags, so a server that
-- already holds the unit id has no other way to see that they moved. The plan
-- itself is deliberately not part of this: it changes on every source edit,
-- and evicting a unit for an edit would throw away modules that a compile-only
-- build will not rebuild.
unitFlagsFingerprint :: Maybe OsPath -> IO Fingerprint
unitFlagsFingerprint = maybe (pure fingerprint0) (getFileHash . fromOsPath)

-- | A stamp over a file's modification time and size, which is what decides
-- whether a cached fingerprint still describes it. A file that is absent
-- stamps as 'fingerprint0', so appearing or disappearing moves the stamp.
fileStamp :: FilePath -> IO Fingerprint
fileStamp "" = pure fingerprint0
fileStamp path = do
  mtime <- modificationTimeIfExists path
  size <- tryMost (getFileSize path)
  pure (fingerprintString (show (mtime, either (const (-1)) id size)))

-- | The flags fingerprint for every unit a plan names as a dependency, reusing
-- the fingerprint from a previous request where neither the dependency's plan
-- file nor the args file it names has moved.
--
-- The uncached walk is the expensive shape: on a monorepo spec binary with
-- 8,621 dependencies it decodes 62.6 MiB of plan JSON and hashes 649.1 MiB of
-- args files on every compile request, and it runs under the state lock on a
-- path that otherwise does no work. That is 70 s with a cold page cache and
-- 0.56 s warm. Two stats a dependency instead is about 35 ms.
dependencyUnitFlags ::
  M.Map FilePath (FilePath, Fingerprint, Fingerprint) ->
  Maybe OsPath ->
  IO ([(UnitId, Fingerprint)], M.Map FilePath (FilePath, Fingerprint, Fingerprint))
dependencyUnitFlags cache0 = \case
  Nothing -> pure ([], cache0)
  Just file -> do
    CachedBuildPlans plans <- decodeJsonArg "--home-unit" file
    foldM step ([], cache0) plans <&> \ (flags, cache) -> (reverse flags, cache)
  where
    step (acc, cache) CachedBuildPlan {name = JsonFs dep, build_plan} = do
      let planPath = fromOsPath build_plan
      planStamp <- fileStamp planPath
      hit <- case M.lookup planPath cache of
        Just (argsPath, stamp, flags) -> do
          argsStamp <- fileStamp argsPath
          pure (if stamp == fingerprintFingerprints [planStamp, argsStamp] then Just flags else Nothing)
        Nothing -> pure Nothing
      case hit of
        Just flags -> pure ((dep, flags) : acc, cache)
        Nothing -> do
          CachedUnit {unit_args} <- decodeJsonBuildPlan build_plan
          let argsPath = maybe "" fromOsPath unit_args
          flags <- unitFlagsFingerprint unit_args
          argsStamp <- fileStamp argsPath
          let stamp = fingerprintFingerprints [planStamp, argsStamp]
          pure ((dep, flags) : acc, M.insert planPath (argsPath, stamp, flags) cache)
