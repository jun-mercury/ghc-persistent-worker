-- | Which entries of a directory of servers a client may send a request to.
--
-- A server keeps three kinds of file beside its socket @<name>@: the slot
-- locks @<name>.lock@ and @<name>.lock.<i>@, and the working-directory
-- handoff socket @<name>.cwd@ (see "GhcWorker.CwdHandoff" in the server).
-- The handoff socket is a socket too, so a listing that kept every socket
-- offered it as a server: a client that found every slot of the real server
-- busy moved on to @<name>.cwd@, locked @<name>.cwd.lock@ and sent gRPC to
-- the handoff listener, which answers no gRPC, and the action failed with
-- exit 76 as if a server had died mid-request.
module GhcWorkerClient.Sockets (
  isServerSocketName,
  serverSockets,
) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf, isSuffixOf, sort)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSocket)

isServerSocketName :: FilePath -> Bool
isServerSocketName name =
  not (".cwd" `isSuffixOf` name) && not (".lock" `isInfixOf` name)

-- | Sorted by name, which is the order clients try servers in, so that they
-- fill the first server's slots before they spread to the next.
serverSockets :: FilePath -> IO [FilePath]
serverSockets dir = do
  entries <- sort . filter isServerSocketName <$> listDirectory dir
  concat <$> traverse (\ e -> socketOnly (dir </> e)) entries
  where
    socketOnly path = do
      status <- try (getFileStatus path)
      pure case status of
        Right st | isSocket st -> [path]
        Right _ -> []
        Left (_ :: IOException) -> []
