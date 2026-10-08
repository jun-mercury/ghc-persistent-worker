-- | A server that serves one build for its whole life.
--
-- A server keeps unit state between requests: the module graph, the home unit
-- graph, the interpreter and what it has linked. A build of one commit that
-- reaches a server holding another commit's state can be served from it, and
-- a compile-only request, whose metadata action the action cache answered
-- elsewhere, has nothing to correct it: a Template Haskell splice then reads
-- the earlier commit's value, and the action's outputs are cached under the
-- later commit's key.
--
-- So the first request binds the server to its @$GHC_WORKER_BUILD_KEY@ and the
-- server serves that key and no other. A request with another key gets
-- 'wrongBuildExit' without running, and the server retires the way a cap
-- retires it (see "GhcWorker.Caps"): it leaves its socket, answers the requests
-- in flight and exits, and whoever started it starts a fresh one, which the
-- next build binds. A process that has exited is the only reset of that state
-- this relies on.
--
-- The key is not part of the action. The remote-execution backend sets it in
-- the action's environment from the invocation, outside the cached action, so
-- binding a server changes no action's digest.
--
-- A request without the key is bound as the empty key, so a pool where nothing
-- sets it serves every build from the same servers, as before, and a server
-- never serves a keyed build and an unkeyed one. A pool that relies on the
-- binding starts its servers with @--require-build-key@, so that a backend
-- that stopped setting the key fails each compile with 'missingBuildExit'
-- rather than quietly sharing servers between builds again.
--
-- The bound key is written beside the socket, @<socket>.build@, so a client can
-- prefer a server already bound to its build, and rewritten on every request
-- served, so its modification time tells a client which server bound to
-- another build was used least recently; see "GhcWorkerClient.Sockets". The
-- file is a hint. Admission is decided here.
module GhcWorker.BuildKey where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (IOException, try)
import Data.Functor (void)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GhcWorker.Caps (Retirement (..))
import System.Directory (renameFile)
import Types.Grpc (CommandEnv (..))

buildKeyVar :: String
buildKeyVar = "GHC_WORKER_BUILD_KEY"

-- | The exit code of a request refused because the server holds another
-- build. Beside the client's 75 (no server) and 76 (server lost); the client
-- moves on to another server when it reads it.
wrongBuildExit :: Int32
wrongBuildExit = 77

-- | The exit code of a request that names no build, at a server started with
-- @--require-build-key@. The client does not retry it: no server would take it.
missingBuildExit :: Int32
missingBuildExit = 78

bindingPath :: FilePath -> FilePath
bindingPath socket = socket ++ ".build"

data BuildBinding =
  BuildBinding {
    -- | Whether a request without a key is refused rather than bound as the empty key.
    required :: Bool,
    -- | The socket the binding file sits beside.
    socket :: FilePath,
    key :: TVar (Maybe String)
  }

newBuildBinding :: Bool -> FilePath -> IO BuildBinding
newBuildBinding required socket = BuildBinding required socket <$> newTVarIO Nothing

data Admission =
  -- | The server was unbound, and this request binds it.
  Binds |
  -- | The server is bound to this request's build.
  Serves |
  -- | The server is bound to the build named.
  Refuses String |
  -- | The request names no build, and the server requires one.
  Unkeyed
  deriving stock (Eq, Show)

admission :: Bool -> Maybe String -> String -> Admission
admission required held key
  | required, null key = Unkeyed
  | otherwise = case held of
      Nothing -> Binds
      Just b
        | b == key -> Serves
        | otherwise -> Refuses b

requestKey :: CommandEnv -> String
requestKey env = fromMaybe "" (Map.lookup buildKeyVar env.values)

-- | Admit a request to the handler only if its build is the server's. The
-- refusal does not count as a request in "GhcWorker.Caps", so it goes outside
-- 'GhcWorker.Caps.capped'.
bound :: BuildBinding -> Retirement -> GrpcHandler -> GrpcHandler
bound binding retirement handler =
  GrpcHandler \ env args -> do
    let key = requestKey env
    decision <- atomically do
      decision <- admission binding.required <$> readTVar binding.key <*> pure key
      case decision of
        Binds -> writeTVar binding.key (Just key)
        Serves -> pure ()
        Refuses held -> modifyTVar' retirement.reason (maybe (Just (refusal held key)) Just)
        Unkeyed -> pure ()
      pure decision
    case decision of
      Binds -> do
        writeBinding binding.socket key
        handler.run env args
      Serves -> do
        writeBinding binding.socket key
        handler.run env args
      Refuses held ->
        pure (["ghc-worker: " ++ refusal held key ++ "; this server serves one build and is retiring"], wrongBuildExit)
      Unkeyed ->
        pure (["ghc-worker: the request sets no " ++ buildKeyVar ++ ", and this server, started with --require-build-key, serves only a named build"], missingBuildExit)
  where
    refusal held key = "build " ++ shown key ++ " sent to a server bound to build " ++ shown held
    shown k = if null k then "(none)" else k

-- | Through a rename, so a client never reads half a key. A failure to write it
-- costs a client its preference and nothing else.
writeBinding :: FilePath -> String -> IO ()
writeBinding socket key =
  void $ try @IOException do
    let tmp = bindingPath socket ++ ".tmp"
    writeFile tmp key
    renameFile tmp (bindingPath socket)
