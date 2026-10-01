-- | Clients hand the server their working directory as an open descriptor.
--
-- A server shared by the actions of a remote executor cannot reach an action's
-- execution root by its path: each action runs in a container of its own that
-- sees its root at one fixed path, so the path a client sends is the same for
-- every action, and the server, outside those containers, finds none of them
-- there. A descriptor of the directory does not depend on what the directory
-- is called in either mount namespace, so the client opens its working
-- directory, sends the descriptor over a unix socket beside the server's own
-- (@<socket>.cwd@) with @SCM_RIGHTS@, and gets back a token, which its request
-- names in @GHC_WORKER_CWD_FD@ (see "GhcWorker.RequestCwd").
--
-- The server keeps the descriptor while the client keeps the handoff
-- connection open, which it does until it exits, and closes it when the
-- connection ends. So a descriptor lives exactly as long as the action that
-- handed it over, and a client killed mid-request leaves nothing behind.
--
-- The handoff runs on its own socket, not in the gRPC request, because the
-- gRPC client owns its connection and sends no ancillary data.
module GhcWorker.CwdHandoff (
  CwdRegistry,
  newCwdRegistry,
  lookupCwd,
  cwdSocketPath,
  serveCwdHandoff,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, stateTVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forever, void)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), accept, bind, close, defaultProtocol, listen, recvFd, socket)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (..))

data CwdRegistry =
  CwdRegistry {
    openFds :: TVar (Map.Map String Fd),
    next :: TVar Int
  }

newCwdRegistry :: IO CwdRegistry
newCwdRegistry = CwdRegistry <$> newTVarIO Map.empty <*> newTVarIO 0

lookupCwd :: CwdRegistry -> String -> IO (Maybe Fd)
lookupCwd registry token = Map.lookup token <$> readTVarIO registry.openFds

cwdSocketPath :: FilePath -> FilePath
cwdSocketPath server = server ++ ".cwd"

-- | Accept handoffs on the socket at the path until the process ends. Each connection carries one descriptor, answered
-- with its token and a newline; the descriptor is closed when the client closes the connection.
serveCwdHandoff :: CwdRegistry -> FilePath -> IO ()
serveCwdHandoff registry path = do
  void (try @IOException (removeFile path))
  bracket (socket AF_UNIX Stream defaultProtocol) close \ listener -> do
    bind listener (SockAddrUnix path)
    listen listener 128
    forever do
      (conn, _) <- accept listener
      void (forkIO (handoff conn `finally` close conn))
  where
    handoff :: Socket -> IO ()
    handoff conn = do
      fd <- Fd <$> recvFd conn
      token <- atomically do
        n <- stateTVar registry.next \ n -> (n, n + 1)
        let token = show n
        modifyTVar' registry.openFds (Map.insert token fd)
        pure token
      sendAll conn (BS.pack (token ++ "\n"))
      -- The client sends nothing more; the read returns when it closes the connection or exits.
      void (try @IOException (drain conn))
      atomically (modifyTVar' registry.openFds (Map.delete token))
      closeFd fd

    drain conn = do
      chunk <- recv conn 4096
      if BS.null chunk then pure () else drain conn
