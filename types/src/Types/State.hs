module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.Target (TargetSpec)
import System.OsPath (OsPath)

data BinPath =
  BinPath {
    initial :: Maybe OsPath,
    extra :: Set OsPath
  }
  deriving stock (Eq, Show)

data Options =
  Options {
    extraGhcOptions :: String,
    -- | Run by a make-mode request after it has restored its state and released the lock, before it compiles. Tests
    -- hold a request here to interleave another one with it deterministically; the server leaves it a no-op.
    afterRestore :: IO ()
  }

defaultOptions :: Options
defaultOptions =
  Options {
    extraGhcOptions = "",
    afterRestore = pure ()
  }

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    options :: Options,
    make :: MakeState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }
