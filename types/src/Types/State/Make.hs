{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import GHC (ModuleGraph, ModuleName, emptyMG)
import GHC.Data.Graph.Directed (Node)
import GHC.Runtime.Interpreter (Interp)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import GHC.Fingerprint (Fingerprint)
import GHC.Unit.Types (UnitId)
import Data.IntMap qualified as IM
import Data.Word (Word64)
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.OsPath (OsPath)

#if defined(UNIT_INDEX)

import GHC.Unit.State (UnitIndex)

#else

data UnitIndex = UnitIndex

#endif

type LibName = String

-- | Currently requested and loaded dynamic libraries
--   which are being loaded via direct loadDLL calls.
--   Loaded libraries are tracked and loading is done only once.
data LibLoadState =
  LibLoadState {
    requested :: M.Map UnitId ([FilePath], [LibName]),
    loaded :: S.Set LibName
  }

emptyLibLoadState :: LibLoadState
emptyLibLoadState = LibLoadState
  { requested = M.empty,
    loaded = S.empty
  }

-- | Maps among unique key, graph idx and graph node content.
-- This is actual content of graph and integerization of nodes
-- for efficient query.
-- In our case, node = ModuleGraphNode
-- Note that Node Int inode = SummaryNode
data KeyIndexNodeMap node = KIN
  { keyIdxMap :: M.Map NodeKey Int,
    idxNodeMap :: IM.IntMap node,
    -- | key to (idx, graph node) pair.
    keyINodeMap :: M.Map NodeKey (Node Int node),
    idxKeyMap :: IM.IntMap NodeKey,
    reachabilityMap :: IM.IntMap IS.IntSet
  }

emptyKINMap :: KeyIndexNodeMap ModuleGraphNode
emptyKINMap = KIN
  { keyIdxMap = M.empty,
    idxNodeMap = IM.empty,
    keyINodeMap = M.empty,
    idxKeyMap = IM.empty,
    reachabilityMap = IM.empty
  }

data EModuleGraph = EModuleGraph
  { moduleGraph :: ModuleGraph,
    keyIndexNodeMap :: KeyIndexNodeMap ModuleGraphNode
  }

emptyEModuleGraph :: EModuleGraph
emptyEModuleGraph = EModuleGraph
  { moduleGraph = emptyMG,
    keyIndexNodeMap = emptyKINMap
  }

-- | Hashes of the files a unit was restored from. A later request that names byte-identical files is spared the
-- decode and reparse; a differing hash sends the request to the semantic check.
data PlanFiles =
  PlanFiles {
    plan :: Fingerprint,
    args :: Maybe (OsPath, Fingerprint)
  }
  deriving stock (Eq, Show)

-- | What a stored unit was built from, so the worker can decide whether the kept 'HomeUnitEnv' may serve a request the
-- way GHC's @checkOldIface@ decides whether an existing interface may be reused. The unit id keys 'hug'; this record is
-- the validity check on that entry. A stored unit that predates this field has no record and is never trusted.
data UnitFingerprint =
  UnitFingerprint {
    -- | GHC's own flag fingerprints over the unit's parsed args, plus the package flags, package databases and GHC
    -- libdir that GHC leaves out of them but that the worker builds the unit state from. Paths GHC excludes for
    -- recompilation, @-odir@ and @-hidir@ among them, do not move it, so two execution roots do not evict each other.
    flags :: Fingerprint,
    modules :: S.Set ModuleName,
    planFiles :: Maybe PlanFiles
  }
  deriving stock (Eq, Show)

-- | A home module by unit and name. 'GHC.Unit.Module' compares by unique, so maps key on this pair instead.
type HomeModuleKey = (UnitId, ModuleName)

-- | Which code a home module stands for: GHC's source, ABI and flag hashes of its interface, combined. Two requests
-- whose dependency closures agree on the code version of every module they share may run their splices in one
-- interpreter; see 'SharedInterp'.
type CodeVersion = Fingerprint

-- | An interpreter shared by the requests whose splices may run in it.
--
-- GHC's loader links a home module into an interpreter once and skips it on every later link, so a request that runs a
-- splice in an interpreter where another commit's version of one of its dependencies is loaded runs that other code.
-- An interpreter therefore records the code version of every module linked into it by a request that has finished,
-- and each running request's claim, the code versions of its dependency closure, which is all its splices can link.
-- A request joins an interpreter only when its claim agrees with both on every module they share; otherwise it runs
-- in an interpreter of its own, and the pool keeps a few so that alternating builds of two commits each find theirs.
data SharedInterp =
  SharedInterp {
    key :: Int,
    interp :: Interp,
    linked :: M.Map HomeModuleKey CodeVersion,
    -- | The running requests' claims, by request token.
    claims :: IM.IntMap (M.Map HomeModuleKey CodeVersion),
    -- | The token of the last request that joined, for evicting the least recently used idle interpreter.
    lastUsed :: Int
  }

data InterpPool =
  InterpPool {
    interps :: [SharedInterp],
    nextKey :: Int
  }

emptyInterpPool :: InterpPool
emptyInterpPool = InterpPool {interps = [], nextKey = 0}

-- | Data extracted from 'HscEnv' for the purpose of persisting it across sessions.
--
-- While many parts of the session are either contained in mutable variables or trivially reinitialized, some components
-- must be handled explicitly: The module graph and home unit graph are pure fields that need to be shared, and the
-- interpreter state for TH execution is only initialized when the flags are parsed.
data MakeState =
  MakeState {
    -- | The module graph for a specific unit is computed in its metadata step, after which it's extracted and merged
    -- into the existing graph.
    moduleGraphState :: EModuleGraph,

    -- | moduleGraph nodes indexed by NodeKey.
    moduleGraphNodes :: M.Map NodeKey ModuleGraphNode,

    -- | The unit environment for a specific unit is inserted into the shared home unit graph at the beginning of the
    -- metadata step, constructed from the dependency specifications provided by Buck.
    -- After compilation of a module, its 'HomeUnitInfo' is inserted into the home package table contained in its unit's
    -- unit environment.
    hug :: HomeUnitGraph,

    -- | The interpreters requests run their splices in. Each session starts with a fresh one from its flags; the
    -- request adopts one of these instead when its dependency closure agrees with it, see 'SharedInterp'.
    interps :: InterpPool,

    unitIndex :: UnitIndex,

    -- | Load locks of the interfaces restored into the home package tables, by unit and module, since two units may
    -- have modules of the same name.
    bcoLoadState :: M.Map HomeModuleKey (MVar ()),

    -- | The args bytes each kept unit was built with, so that a later request
    -- naming the same unit id can tell redefined flags from the unit in memory
    -- without parsing them again.
    unitPlans :: M.Map UnitId Fingerprint,

    -- | Unit-level extra native library dependencies are loaded by checking in LibLoadState explicitly.
    extraLib :: LibLoadState,

    -- | What each stored unit was built from, keyed by the same UnitId as 'hug'. A request for a known unit is served
    -- from 'hug' only when this record still matches the plan; a unit absent here is not trusted.
    unitFingerprints :: M.Map UnitId UnitFingerprint,

    -- | The generation of each unit in 'hug': a fresh number whenever a unit env is inserted, gone when the unit is
    -- evicted. A request remembers the generations it restored, and writes its work on a unit back only if that unit
    -- still has the generation it restored, so a request never writes over a unit another request replaced meanwhile.
    unitGenerations :: M.Map UnitId Word64,

    nextGeneration :: Word64,

    -- | Numbers the requests, for their claims on 'interps'.
    nextRequest :: Int
  }
