-- | Description: A server serves the build of its first request and no other. A request for another build is refused
-- without reaching the handler and retires the server, and a server that requires a key refuses a request without one.
-- The handler here stands in for GHC: what is tested is who reaches it.
module BuildKeyTest where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import GhcWorker.BuildKey (Admission (..), admission, bindingPath, bound, missingBuildExit, newBuildBinding, wrongBuildExit)
import GhcWorker.Caps (Retirement (..), newRetirement)
import Hedgehog (TestT, assert, (===))
import Data.Functor (void)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Run (unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)
import Types.Grpc (CommandEnv (..), RequestArgs (..))

test_admission :: TestT IO ()
test_admission = do
  admission False Nothing "a" === Binds
  admission False (Just "a") "a" === Serves
  admission False (Just "a") "b" === Refuses "a"
  -- No key is a build of its own: it shares a server with other unkeyed requests and with nothing keyed.
  admission False Nothing "" === Binds
  admission False (Just "") "" === Serves
  admission False (Just "") "a" === Refuses ""
  admission False (Just "a") "" === Refuses "a"
  admission True Nothing "" === Unkeyed
  admission True (Just "a") "" === Unkeyed
  admission True Nothing "a" === Binds

-- | Builds a, a, b, a in turn reach one server. b is refused with 77 without reaching the handler and sets the retirement
-- reason; the a after it is still served, since the server keeps its binding while it retires.
test_oneBuild :: IO FilePath -> TestT IO ()
test_oneBuild tmpResource = do
  tmp <- liftIO tmpResource
  let socket = tmp </> "0-1"
  (codes, served, reason, file) <- liftIO do
    binding <- newBuildBinding False socket
    retirement <- newRetirement
    calls <- newIORef []
    let handler = GrpcHandler \ env _ -> do
          modifyIORef' calls (Map.lookup "GHC_WORKER_BUILD_KEY" env.values :)
          pure ([], 0)
        server = bound binding retirement handler
        send key = snd <$> server.run (CommandEnv (Map.fromList [("GHC_WORKER_BUILD_KEY", key)])) (RequestArgs ["--unit", "u"])
    codes <- traverse @[] send ["a", "a", "b", "a"]
    (,,,) codes <$> (reverse <$> readIORef calls) <*> readTVarIO retirement.reason <*> readFile (bindingPath socket)
  codes === [0, 0, wrongBuildExit, 0]
  served === [Just "a", Just "a", Just "a"]
  reason === Just "build b sent to a server bound to build a"
  file === "a"

-- | Every request a server serves rewrites its binding file, whose modification time clients read as when the server was
-- last used: removed between two requests of the bound build, the file is back after the second.
test_servedRewritesBinding :: IO FilePath -> TestT IO ()
test_servedRewritesBinding tmpResource = do
  tmp <- liftIO tmpResource
  let socket = tmp </> "2-1"
  (before, after) <- liftIO do
    binding <- newBuildBinding False socket
    retirement <- newRetirement
    let server = bound binding retirement (GrpcHandler \ _ _ -> pure ([], 0))
        send = void (server.run (CommandEnv (Map.fromList [("GHC_WORKER_BUILD_KEY", "a")])) (RequestArgs []))
    send
    removeFile (bindingPath socket)
    before <- doesFileExist (bindingPath socket)
    send
    (,) before <$> doesFileExist (bindingPath socket)
  assert (not before)
  assert after

test_required :: IO FilePath -> TestT IO ()
test_required tmpResource = do
  tmp <- liftIO tmpResource
  let socket = tmp </> "1-1"
  (codes, reason, bindingWritten) <- liftIO do
    binding <- newBuildBinding True socket
    retirement <- newRetirement
    let server = bound binding retirement (GrpcHandler \ _ _ -> pure ([], 0))
    codes <- traverse @[] (\ env -> snd <$> server.run (CommandEnv (Map.fromList env)) (RequestArgs [])) [[], [("GHC_WORKER_BUILD_KEY", "")], [("GHC_WORKER_BUILD_KEY", "a")]]
    (,,) codes <$> readTVarIO retirement.reason <*> doesFileExist (bindingPath socket)
  -- A request without a key binds nothing and retires nothing; the next, keyed one binds the server.
  codes === [missingBuildExit, missingBuildExit, 0]
  reason === Nothing
  assert bindingWritten

test_buildKey :: TestTree
test_buildKey =
  withTemp "build-key" \ tmp ->
    testGroup "a server serves one build" [
      unitTest "admission by the bound key" test_admission,
      unitTest "a second build is refused and retires the server" (test_oneBuild tmp),
      unitTest "a server that requires a key refuses a request without one" (test_required tmp),
      unitTest "a served request rewrites the binding file" (test_servedRewritesBinding tmp)
    ]
