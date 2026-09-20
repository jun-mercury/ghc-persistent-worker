{-# LANGUAGE CPP, OverloadedLists, PatternSynonyms #-}

module Internal.Cache.Hpt where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), execStateT, get, put)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Map.Strict qualified as M (Map, insert, lookup)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set (insert, member, singleton)
import Data.Time (getCurrentTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC (DynFlags, GhcException (..), IsBootInterface (..), ModIface, ModIface_ (..), ModLocation (..), Module, ModuleName, mkModule, mkModuleName, moduleName, moduleNameString)
import GHC.Data.Bag (emptyBag)
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
import GHC.Unit.Home.PackageTable (HomePackageTable, addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Module (moduleNameSlashes)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey (..))
import GHC.Unit.Module.Location (addBootSuffix, pattern ModLocation)
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Module.ModIface (IfaceTopEnv (..), set_mi_top_env)
import GHC.Unit.Module.WholeCoreBindings (WholeCoreBindings (..))
import GHC.Utils.Misc (modificationTimeIfExists)
import GHC.Utils.Outputable (ppr, ($+$))
import GHC.Utils.Panic (throwGhcExceptionIO, tryMost)
import Internal.AbiHash (showAbiHash)
import Internal.Cache.Metadata (loadCachedHomeUnit, loadCachedDepUnits, readParseGHCArgs)
import Internal.Compat.FixedNodes (pattern CompileNode, pattern FixedNode, deps)
import Internal.Compat.GHC914 (edgeTarget, setExtraDecls)
import Internal.Log (logTimed)
import Data.Char (isSpace)
import Prelude hiding (log)
import System.FilePath ((<.>), (</>))
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import Types.BuckArgs (IsInterpreted (Compiled, Interpreted), decodeJsonArg)
import Types.CachedDeps (CachedDep (..), CachedDeps (..), CachedUnit (..), JsonFs (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (make))
import Types.State.Make (bcoLoadState)

#if !defined(LINKABLES)

import GHC.Utils.Outputable (text)

#endif

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

-- | Decide how a dependency module gets into the home package table.
--
-- An entry already in the table is keyed by module name, and the table is
-- shared by every request the server serves. Two clients whose project roots
-- hold different versions of the same module (two execution roots on a
-- remote-execution machine, two builds of two branches) would otherwise alias:
-- the second would compile against whatever the first had loaded. So an
-- existing entry is checked against the ABI hash the compile of that module
-- wrote beside its interface (@--abi-out@, the interface path plus @.hash@,
-- read relative to the request's working directory): if the file is there and
-- differs, the entry is stale and the interface is reloaded from disk, whoever
-- loaded it before. A missing hash file leaves the entry trusted, which is
-- what every entry was before this check.
prepareHmiLoader ::
  Logger ->
  HscEnv ->
  ModuleName ->
  OsPath ->
  StateT WorkerState IO ModuleLoadState
prepareHmiLoader log hsc_env name ifaceFile = do
  existing <- liftIO (lookupHpt hpt name)
  stale <- liftIO (maybe (pure False) staleOnDisk existing)
  case existing of
    Just hmi | not stale ->
      case homeModInfoByteCode hmi of
        Just _ -> pure Loaded
        Nothing -> updateBcoState False
    Just _ -> do
      liftIO $ log.debug ("HPT entry for " ++ moduleNameString name ++ " differs from " ++ hashFile ++ ", reloading")
      updateBcoState True
    Nothing -> updateBcoState False
  where
    hpt = hsc_HPT hsc_env

    hashFile = fromOsPath ifaceFile ++ ".hash"

    staleOnDisk hmi =
      try (readFile hashFile) <&> \case
        Left (_ :: IOException) -> False
        Right onDisk -> strip onDisk /= strip (showAbiHash hsc_env hmi.hm_iface)

    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    -- A stale entry takes a fresh lock even if one exists, so the reload
    -- happens rather than a wait on the load that produced the stale entry.
    updateBcoState stale = do
      new_lock <- liftIO newEmptyMVar
      s <- get
      let make = s.make
          m = make.bcoLoadState
          mlock = M.lookup name m
      case mlock of
        Just lock | not stale -> pure (Waiting lock)
        _ -> do
          let m' = M.insert name new_lock m
              make' = make {bcoLoadState = m'}
          put s {make = make'}
          pure (RequestHi new_lock)

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
          if features.lazyByteCode
#if defined(LINKABLES)
          then pure Nothing
#else
          then throwGhcExceptionIO $
            PprProgramError
             "ghc-worker error"
             (text "features.lazyByteCode is on, but buck-worker-internal is not compiled with -flinkables")
#endif
          else loadCachedByteCode hsc_env (fromOsPath ifaceFile) hm_iface hm_details
        let hm_iface' = (if features.lazyByteCode then id else setExtraDecls Nothing) hm_iface
        let hmi' = HomeModInfo {
          hm_iface = hm_iface',
          hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode},
          hm_details
        }
        addHomeModInfoToHpt hmi' hpt

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
    (state1, hsc_env1) <- foldM loadDepUnit (state0, hsc_env0) byUnit
    pure (state1, hscSetActiveUnitId (hscActiveUnitId hsc_env0) hsc_env1)
  where
    -- If the unit isn't present in the unit env, it wasn't built by a worker, since it would have been loaded in the
    -- metadata restoration step.
    loadDepUnit (state, hsc_env) mods@(CachedDep {package = JsonFs uid} :| _) =
      if hasUnit uid hsc_env
      then do
        let hsc_env' = hscSetActiveUnitId uid hsc_env
        (,hsc_env') <$> loadActiveUnit hsc_env' state (toList mods)
      else pure (state, hsc_env)

    loadActiveUnit :: HscEnv -> WorkerState -> [CachedDep] -> IO WorkerState
    loadActiveUnit hsc_env state mods =
      flip execStateT state do
        mod_plans <- traverse (prepareDep hsc_env) mods
        liftIO $ for_ mod_plans \ (name, iface, mod_load_state) -> do
          mod_load_state' <- loadCachedDep log features interp hsc_env name iface mod_load_state
          loadCachedDep log features interp hsc_env name iface mod_load_state'

    prepareDep hsc_env CachedDep {name = JsonFs name, package = JsonFs uid} = do
      iface <- maybe (missingHiDir uid name) pure (canonicalInterfacePath (hsc_dflags hsc_env) name)
      mod_load_state <- prepareHmiLoader log hsc_env name iface
      pure (name, iface, mod_load_state)

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
loadHomeUnit log dflags0 features unit (state0, hsc_env0) path
  | hasUnit unit hsc_env0
  = pure (state0, hsc_env0)
  | otherwise
  = do
    cachedUnit@CachedUnit {unit_args} <- decodeJsonArg "--home-unit" path
    (state1, hsc_env1) <- fmap (fromMaybe (state0, hsc_env0)) $ for cachedUnit.dep_units \ file -> do
      deps <- decodeJsonArg "--home-unit" file
      loadCachedDepUnits log dflags0 deps features (state0, hsc_env0)
    dflags <- maybe (pure dflags0) (readParseGHCArgs features.flagParser hsc_env1 dflags0) unit_args
    logTimed log "Loading cached home unit" $ fmap swap do
      runStateT (loadCachedHomeUnit log features.fixedNodesCache hsc_env1 unit (cachedUnit, dflags)) state1
