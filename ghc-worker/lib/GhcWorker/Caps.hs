-- | A server that retires itself past a request count or a resident-set size.
--
-- A @--jobs 1@ server on a remote-execution machine grows 8 to 9 MB per
-- request and gives nothing back: on a 32 GB machine four of them reached the
-- kernel's OOM killer after about 1,250 requests on the busiest socket, and a
-- killed server leaves a socket nobody answers and a request nobody finishes.
-- So the server takes two caps on its command line, @--max-requests@ and
-- @--max-rss-mb@, and when a request ends past either it stops accepting,
-- removes its socket file and exits 0 once that request's client has read the
-- response; whoever started it (the boot script's loop) starts a fresh one on
-- the same path. Both caps are off unless given.
module GhcWorker.Caps where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Exception (IOException, bracket_, try)
import Control.Monad (void)
import Common.Grpc (GrpcHandler (..))
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import Internal.Log (dbg)
import System.Directory.OsPath (doesFileExist, removeFile)
import System.IO (SeekMode (..), readFile')
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import System.Posix.IO (LockRequest (..), OpenMode (..), closeFd, defaultFileFlags, openFd, setLock)

data Caps =
  Caps {
    maxRequests :: Maybe Int,
    maxRssMb :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Which cap a server has reached after @requests@ requests with @rssKb@
-- resident, if any, worded for the log line.
capReached :: Caps -> Int -> Int -> Maybe String
capReached Caps {maxRequests, maxRssMb} requests rssKb
  | Just n <- maxRequests, requests >= n = Just ("request cap " ++ show n)
  | Just mb <- maxRssMb, rssKb >= mb * 1024 = Just ("memory cap " ++ show mb ++ " MB, VmRSS " ++ show (rssKb `div` 1024) ++ " MB")
  | otherwise = Nothing

-- | @VmRSS@ in kB from the text of @/proc/self/status@.
vmRssKb :: String -> Maybe Int
vmRssKb status =
  listToMaybe [n | line <- lines status, Just rest <- [stripPrefix "VmRSS:" line], (n, _) <- reads rest]

-- | The process's resident set, or 'Nothing' where there is no procfs.
readVmRssKb :: IO (Maybe Int)
readVmRssKb =
  try (readFile' "/proc/self/status") >>= \case
    Right status -> pure (vmRssKb status)
    Left (_ :: IOException) -> pure Nothing

data Retirement =
  Retirement {
    requests :: TVar Int,
    inFlight :: TVar Int,
    reason :: TVar (Maybe String)
  }

newRetirement :: IO Retirement
newRetirement = Retirement <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO Nothing

-- | Count a request, and after it decide whether the server retires. The
-- request's own response still goes out: the wrapper returns it, and
-- 'awaitRetirement' waits for the client to have read it.
capped :: Caps -> Retirement -> GrpcHandler -> GrpcHandler
capped caps retirement handler =
  GrpcHandler \ commandEnv argv ->
    bracket_ (count retirement.inFlight 1) (count retirement.inFlight (-1)) do
      out <- handler.run commandEnv argv
      n <- atomically do
        modifyTVar' retirement.requests (+ 1)
        readTVar retirement.requests
      rss <- readVmRssKb
      for_ (capReached caps n (fromMaybe 0 rss)) \ reached ->
        atomically $ modifyTVar' retirement.reason (maybe (Just reached) Just)
      pure out
  where
    count var d = atomically (modifyTVar' var (+ d))

-- | Block until a cap is reached, then take the server off its socket and
-- return when the last client has its response. Removing the socket file is
-- what stops new clients: one that already locked this server finds no
-- socket, releases the lock and moves on. The response to the request that
-- reached the cap is on its way when the handler returns; the client reads
-- it and exits, and the directory-mode client holds the @fcntl@ lock on
-- @<socket>.lock@ until it exits, so that lock coming free is the protocol's
-- own signal that the response arrived. Without a lock file, a client that
-- was given the socket path directly, a second is left for the same.
awaitRetirement :: Retirement -> OsPath -> IO ()
awaitRetirement retirement socket = do
  reached <- atomically $ readTVar retirement.reason >>= maybe retry pure
  n <- readTVarIO retirement.requests
  dbg ("ghc-worker: " ++ reached ++ " reached after " ++ show n ++ " requests; removing " ++ path ++ " and exiting when the request in flight is answered")
  void $ try @IOException (removeFile socket)
  atomically $ readTVar retirement.inFlight >>= check . (== 0)
  hasLock <- doesFileExist lockFile
  if hasLock then waitForLock (fromOsPath lockFile) (400 :: Int) else threadDelay 1_000_000
  dbg ("ghc-worker: retired after " ++ show n ++ " requests")
  where
    path = fromOsPath socket
    lockFile = socket <> toOsPath ".lock"

    -- Every 25 ms like the client, and not forever: a client that never exits
    -- is a hang of its own, and ten seconds is longer than any response takes
    -- to cross a unix socket.
    waitForLock _ 0 = dbg "ghc-worker: a client still holds the lock after 10 s; exiting anyway"
    waitForLock file tries = do
      fd <- openFd file WriteOnly defaultFileFlags
      locked <- try @IOException (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
      closeFd fd
      case locked of
        Right () -> pure ()
        Left _ -> do
          threadDelay 25_000
          waitForLock file (tries - 1)
