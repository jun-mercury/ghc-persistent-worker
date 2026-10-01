{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (finally)
import Data.Foldable (for_, traverse_)
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
  HomeModuleKey,
  CodeVersion,
  emptyEModuleGraph,
  emptyInterpPool,
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
      interps = emptyInterpPool,
      unitIndex,
      bcoLoadState,
      extraLib = emptyLibLoadState,
      unitPlans = M.empty,
      unitFingerprints = M.empty,
      unitGenerations = M.empty,
      nextGeneration = 0,
      nextRequest = 0
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

-- | Run a request on the state kept across requests, which other requests use at the same time.
--
-- Under the state lock, restore the kept units, module graph and unit index into the session, run @setup@ (which
-- restores or evicts what this request needs, and loads its dependencies' interfaces into the kept home package
-- tables), and turn the session into a request of its own with 'Make.beginRequest': private copies of the home package
-- tables and an interpreter that agrees with @claim@, the code versions of the modules the request's splices may link.
-- Then release the lock and compile. Afterwards, under the lock again, write back what the compile added to its unit,
-- if no other request replaced that unit meanwhile ('Make.commitRequest'), and, whatever happened, release the claim on
-- the interpreter.
withState ::
  Logger ->
  MVar WorkerState ->
  ((WorkerState, HscEnv) -> IO (WorkerState, HscEnv)) ->
  ((WorkerState, HscEnv) -> IO (M.Map HomeModuleKey CodeVersion)) ->
  Ghc a ->
  Ghc a
withState logger stateVar setup claim prog = do
  hsc_env0 <- getSession
  (hsc_env1, request, afterRestore) <- liftIO (restore hsc_env0)
  setSession hsc_env1
  liftIO afterRestore
  (prog <* withSession (liftIO . commit request)) `finally` liftIO (release request)
  where
    restore hsc_env =
      modifyMVar stateVar \ state -> do
        (state1, hsc_env1) <- setup (state, Make.loadState hsc_env state.make)
        claimed <- claim (state1, hsc_env1)
        (make, request, hsc_env2) <- Make.beginRequest logger claimed hsc_env1 state1.make
        pure (state1 {make}, (hsc_env2, request, state1.options.afterRestore))

    commit request hsc_env =
      withMVar stateVar \ state -> Make.commitRequest logger request hsc_env state.make

    release request =
      for_ request.interp \ (_, interp) -> do
        loaded <- Make.loadedModules interp
        modifyMVar_ stateVar \ state ->
          pure state {make = state.make {interps = Make.leaveInterp request loaded state.make.interps}}

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
