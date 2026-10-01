-- | Description: A client lists only server sockets in a directory of servers. The directory is laid out the way the
-- BuildBuddy sidecar lays it out (@<lane>-<generation>@ with its @.lock@, @.lock.<i>@ and @.cwd@ siblings), with real
-- listening sockets, since the handoff socket being a socket is what made it look like a server.
module SocketsTest where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import GhcWorkerClient.Sockets (isServerSocketName, serverSockets)
import Hedgehog (TestT, assert, (===))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), bind, close, defaultProtocol, listen, socket)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSocket)
import Test.Run (unitTest, withTemp)
import Test.Tasty (TestTree, testGroup)

listening :: FilePath -> IO Socket
listening path = do
  s <- socket AF_UNIX Stream defaultProtocol
  bind s (SockAddrUnix path)
  listen s 1
  pure s

test_sidecarLayout :: IO FilePath -> TestT IO ()
test_sidecarLayout tmpResource = do
  tmp <- liftIO tmpResource
  let dir = tmp </> "ghckey"
      server0 = dir </> "0-3"
      server1 = dir </> "1-1"
  (handoffIsSocket, sockets) <- liftIO do
    createDirectory dir
    bracket (traverse @[] listening [server0, server0 ++ ".cwd", server1, server1 ++ ".cwd"]) (mapM_ close) \ _ -> do
      mapM_ @[] (\ f -> writeFile f "") [server0 ++ ".lock", server0 ++ ".lock.1", server0 ++ ".lock.2", server1 ++ ".lock", dir </> "notes"]
      (,) <$> (isSocket <$> getFileStatus (server0 ++ ".cwd")) <*> serverSockets dir
  assert handoffIsSocket
  sockets === [server0, server1]

test_names :: TestT IO ()
test_names = do
  assert (isServerSocketName "0-3")
  assert (isServerSocketName "s0")
  assert (not (isServerSocketName "0-3.cwd"))
  assert (not (isServerSocketName "0-3.lock"))
  assert (not (isServerSocketName "0-3.lock.7"))
  assert (not (isServerSocketName "0-3.cwd.lock"))

test_sockets :: TestTree
test_sockets =
  withTemp "sockets" \ tmp ->
    testGroup "a client lists only server sockets" [
      unitTest "the sidecar's layout yields its servers, never a handoff socket" (test_sidecarLayout tmp),
      unitTest "handoff and lock names are not servers" test_names
    ]
