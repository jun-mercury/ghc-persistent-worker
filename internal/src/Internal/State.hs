{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import GHC (Ghc, HscEnv, getSession, setSession)
import GHC.Driver.Monad (withSession)
import GHC.Unit.Home.Graph (unitEnv_new)
import Internal.Debug (showHugShort, showModGraph)
import qualified Internal.State.Make as Make
import Internal.State.UnitIndex (newUnitIndex)
import System.Environment (lookupEnv)
import System.OsPath.Extra (toOsPath)
import Types.Log (Logger (..))
import Types.State (BinPath (..), Options (..), WorkerState (..), defaultOptions)
import Types.State.Make (
  EModuleGraph (..),
  MakeState (..),
  emptyEModuleGraph,
  emptyLibLoadState,
  )

newState :: IO (MVar WorkerState)
newState = do
  initialPath <- lookupEnv "PATH"
  unitIndex <- newUnitIndex
  let bcoLoadState = M.empty
  newMVar WorkerState {
    path = BinPath {
      initial = toOsPath <$> initialPath,
      extra = mempty
    },
    baseSession = Nothing,
    options = defaultOptions,
    make = MakeState {
      moduleGraphState = emptyEModuleGraph,
      moduleGraphNodes = M.empty,
      hug = unitEnv_new mempty,
      interp = Nothing,
      unitIndex,
      bcoLoadState,
      extraLib = emptyLibLoadState,
      unitPlans = M.empty,
      unitGenerations = M.empty,
      unitFingerprints = M.empty
    },
    targetArgs = mempty
  }

modifyMakeState :: MVar WorkerState -> (MakeState -> IO (MakeState, a)) -> IO a
modifyMakeState var f =
  modifyMVar var \ state -> do
    (make, a) <- f state.make
    pure (state {make}, a)

-- | Update the 'MakeState' field in the 'WorkerState'.
updateMakeState :: (MakeState -> MakeState) -> WorkerState -> WorkerState
updateMakeState f state = state {make = f state.make}

updateMakeStateVar :: MVar WorkerState -> (MakeState -> MakeState) -> IO ()
updateMakeStateVar var f = modifyMakeState var (\ s -> pure (f s, ()))

-- | Restore the HUG, module graph and interpreter state from the worker state, since those are the only two components
-- modified by the worker that aren't already shared by the base session.
withState ::
  Logger ->
  MVar WorkerState ->
  ((WorkerState, HscEnv) -> IO (WorkerState, HscEnv)) ->
  Ghc a ->
  Ghc a
withState logger stateVar setup prog = do
  restored <- liftIO (newIORef M.empty)
  hsc_env0 <- getSession
  (hsc_env1, afterRestore) <- restore restored hsc_env0
  setSession hsc_env1
  liftIO afterRestore
  prog <* withSession (store restored)
  where
    restore restored hsc_env =
      liftIO $ modifyMVar stateVar \ state -> do
        writeIORef restored state.make.unitGenerations
        (state1, hsc_env1) <- setup (state, Make.loadState hsc_env state.make)
        let (make, hsc_env2) = Make.ensureInterp hsc_env1 state1.make
        pure (state1 {make}, (hsc_env2, state1.options.afterRestore))

    store restored hsc_env =
      liftIO $ modifyMVar_ stateVar \ state -> do
        generations <- readIORef restored
        make <- Make.storeState logger generations hsc_env state.make
        pure state {make}

dumpState ::
  Logger ->
  MVar WorkerState ->
  Maybe String ->
  IO ()
dumpState logger state exception =
  withMVar state \ WorkerState {make = MakeState {moduleGraphState, hug}} -> do
    write "-----------------"
    write "Request failed!"
    traverse_ write exception
    write "-----------------"
    write "Module graph:"
    writeD (showModGraph moduleGraphState.moduleGraph)
    write "-----------------"
    write "Home unit graph:"
    writeD =<< showHugShort hug
  where
    write = logger.debug
    writeD = logger.debugD
