-- | LRU tracking and unloading of lazily-loaded bytecode (the @lazyByteCode@ feature, see 'Types.FeatureFlags').
module Internal.Cache.Bytecode where

import Control.Monad (unless)
import Data.Bifunctor (bimap)
import Data.Foldable (foldr', for_, traverse_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.ByteCode.Types (bc_bcos)
import GHC.Data.FlatBag (sizeFlatBag)
import GHC.Driver.Env (HscEnv, hscInterp)
import GHC.Linker.Loader (unload)
import GHC.Linker.Types (Linkable (..), linkableBCOs, linkableModule)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), UnitEnvGraph (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Types (Module, moduleName, moduleUnitId)
import Control.Concurrent.MVar (MVar)
import GHC.Linker.Types (LoaderState, loader_state)
import GHC.Runtime.Interpreter (Interp, interpLoader)
import GHC.Unit.Types (moduleUnit, toUnitId)
import Types.State.Make (BcoCacheEntry (..), InterpPool (..), MakeState (..), SharedInterp (..))

-- | Count the number of BCOs in a 'Linkable' as a coarse proxy measure for memory usage.
-- Note: this ignores @LazyBCOs@.
linkableBcoCount :: Linkable -> Int
linkableBcoCount lnk =
  sum (fromIntegral . sizeFlatBag . bc_bcos <$> linkableBCOs lnk)

-- | Increase the access counter and update the LRU cache entries for each 'Linkable'.
touchBcoCache :: [Linkable] -> MakeState -> MakeState
touchBcoCache linkables make =
  make {
    bcoCache = foldr' touch make.bcoCache withBcos,
    bcoAccessCounter
  }
  where
    withBcos = filter (not . null . linkableBCOs) linkables

    touch linkable = Map.insert linkable.linkableModule (newEntry linkable)

    newEntry linkable = BcoCacheEntry {linkable, lastAccess = bcoAccessCounter, size = linkableBcoCount linkable}

    bcoAccessCounter = make.bcoAccessCounter + 1

-- | Clear the bytecode field of a module's HPT entry.
dropFromHpt :: UnitEnvGraph HomeUnitEnv -> Module -> IO ()
dropFromHpt hug target =
  for_ unitEnv \ hue -> do
    lookupHpt (homeUnitEnv_hpt hue) (moduleName target) >>= traverse_ \ hmi ->
      addHomeModInfoToHpt (clear hmi) (homeUnitEnv_hpt hue)
  where
    clear hmi = hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode = Nothing}}

    unitEnv = unitEnv_lookup_maybe (moduleUnitId target) hug

-- | Break the list of cache entries where the total size of BCOs exceeds the given number.
splitEvictions :: Int -> [(Module, BcoCacheEntry)] -> ([(Module, BcoCacheEntry)], [(Module, BcoCacheEntry)])
splitEvictions limit entries =
  bimap (fmap fst) (fmap fst) $
    break (\ (_, size) -> size >= limit) $
    zip entries $
    scanl (+) 0 [entry.size | (_, entry) <- entries]

-- | If the tracked total size of 'bcoCache' exceeds the given limit, unload the least recently used excess entries:
--
-- - Clear the bytecode fields in the HPT for the affected modules
-- - Call 'unload' with the remaining modules (that function expects the kept modules, not the unloaded ones)
evictBcoCache :: HscEnv -> Int -> MakeState -> IO MakeState
evictBcoCache hsc_env limit make = do
  unless (null evicted) do
    traverse_ (dropFromHpt make.hug . fst) evicted
    unload (hscInterp hsc_env) hsc_env (linkable . snd <$> kept)
  pure make {bcoCache = Map.fromList kept}
  where
    (kept, evicted) = splitEvictions limit sorted

    sorted = reverse (List.sortOn (lastAccess . snd) (Map.toList make.bcoCache))

-- | 'touchBcoCache' for the pooled interpreter a link ran in. An interpreter is recognised by its loader's state
-- variable, which every copy of the 'Interp' shares; a link into an interpreter the pool does not hold records nothing.
touchInterpBcoCache :: Interp -> [Linkable] -> MakeState -> MakeState
touchInterpBcoCache interp linkables make =
  make {interps = make.interps {interps = touch <$> make.interps.interps}}
  where
    touch e
      | loaderVar e.interp == loaderVar interp =
        e {bcoCache = foldr' (\ l -> Map.insert l.linkableModule (newEntry counter l)) e.bcoCache withBcos, bcoAccessCounter = counter}
      | otherwise = e
      where
        counter = e.bcoAccessCounter + 1

    withBcos = filter (not . null . linkableBCOs) linkables

    newEntry counter linkable = BcoCacheEntry {linkable, lastAccess = counter, size = linkableBcoCount linkable}

loaderVar :: Interp -> MVar (Maybe LoaderState)
loaderVar i = loader_state (interpLoader i)

-- | 'evictBcoCache' for one idle pooled interpreter: unload its least recently used bytecode beyond the limit, clear
-- that bytecode in the kept home package tables, and forget the unloaded modules in the interpreter's record of what is
-- linked into it, so a later request's claim is not held against code that is no longer there.
evictInterpBcoCache :: HscEnv -> Int -> UnitEnvGraph HomeUnitEnv -> SharedInterp -> IO SharedInterp
evictInterpBcoCache hsc_env limit hug e = do
  unless (null evicted) do
    traverse_ (dropFromHpt hug . fst) evicted
    unload e.interp hsc_env (linkable . snd <$> kept)
  pure e {
    bcoCache = Map.fromList kept,
    linked = foldr' (Map.delete . homeKey . fst) e.linked evicted
  }
  where
    (kept, evicted) = splitEvictions limit sorted

    sorted = reverse (List.sortOn (lastAccess . snd) (Map.toList e.bcoCache))

    homeKey m = (toUnitId (moduleUnit m), moduleName m)
