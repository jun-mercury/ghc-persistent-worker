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
--
-- A server bound to a build names it in @<name>.build@ (see
-- "GhcWorker.BuildKey" in the server), so a client tries the servers its own
-- build already warmed before an unbound one, and one bound to another build,
-- which refuses it and retires, last. The server rewrites the file on every
-- request it serves, so its modification time is when the server was last
-- used, and among servers bound to other builds the client takes the least
-- recently used: evicting by name order alone took the first server again for
-- every new build once all were bound, while the others kept the bindings of
-- builds long finished.
module GhcWorkerClient.Sockets (
  Binding (..),
  byBinding,
  isServerSocketName,
  readBinding,
  serverSockets,
) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf, isSuffixOf, sort, sortOn)
import System.Directory (listDirectory)
import System.IO (readFile')
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isSocket, modificationTime)
import System.Posix.Types (EpochTime)

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

-- | The build a server is bound to and when it last served a request.
data Binding =
  Binding {
    key :: String,
    lastUsed :: EpochTime
  }
  deriving stock (Eq, Show)

-- | The binding of a server, 'Nothing' while it is unbound or gone.
readBinding :: FilePath -> IO (Maybe Binding)
readBinding socket = do
  let path = socket ++ ".build"
  try ((,) <$> getFileStatus path <*> readFile' path) >>= \case
    Right (st, key) -> pure (Just Binding {key, lastUsed = modificationTime st})
    Left (_ :: IOException) -> pure Nothing

-- | The servers in the order a client of the given build tries them: bound to
-- it, then unbound, each in the order given, then bound to another build, least
-- recently used first.
byBinding :: String -> [(FilePath, Maybe Binding)] -> [(FilePath, Maybe Binding)]
byBinding build = sortOn (rank . snd)
  where
    rank :: Maybe Binding -> (Int, EpochTime)
    rank = \case
      Just b | b.key == build -> (0, 0)
      Nothing -> (1, 0)
      Just b -> (2, b.lastUsed)
