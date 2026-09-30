module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import System.OsPath (OsPath)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.Target (TargetSpec)

data BinPath =
  BinPath {
    initial :: Maybe OsPath,
    extra :: Set OsPath
  }
  deriving stock (Eq, Show)

-- | Per-server options that are not part of any request. Upstream removed the extra GHC options this record carried
-- (1d24710); the test hook below is what remains.
data Options =
  Options {
    -- | Run by a make-mode request after it has restored its state and released the lock, before it compiles. Tests
    -- hold a request here to interleave another one with it deterministically; the server leaves it a no-op.
    afterRestore :: IO ()
  }

defaultOptions :: Options
defaultOptions =
  Options {
    afterRestore = pure ()
  }

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    make :: MakeState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs),
    options :: Options
  }
