-- | A server that retires itself past a request count or a resident-set size.
--
-- A @--jobs 1@ server on a remote-execution machine grows 8 to 9 MB per
-- request and gives nothing back: on a 32 GB machine four of them reached the
-- kernel's OOM killer after about 1,250 requests on the busiest socket, and a
-- killed server leaves a socket nobody answers and a request nobody finishes.
-- So the server takes caps on its command line, @--max-requests@,
-- @--max-rss-mb@ and @--max-live-mb@, and when a request ends past any of them
-- it stops accepting, removes its socket file and exits 0 once that request's
-- client has read the response; whoever started it (the boot script's loop)
-- starts a fresh one on the same path. Every cap is off unless given.
--
-- @--max-rss-mb@ cannot tell a server's retained state from one compile's
-- peak: the RTS keeps the heap a heavy module needed (22.9 GB for mwb's
-- heaviest) long after the request, so on a pool that serves heavy modules
-- the resident set crosses any cap that leaves room for one compile, and the
-- server retires after every heavy request. @--max-live-mb@ reads the live
-- bytes of the last major collection instead, which is what the kept units,
-- interfaces and module graph hold, and a transient peak does not reach it.
module GhcWorker.Caps where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Exception (IOException, bracket_, try)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Control.Monad (filterM, void)
import Common.Grpc (GrpcHandler (..))
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import Internal.Log (dbg)
import Types.Grpc (RequestArgs (..))
import System.Directory.OsPath (doesFileExist, removeFile)
import System.IO (SeekMode (..), readFile')
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import System.Posix.IO (LockRequest (..), OpenMode (..), closeFd, defaultFileFlags, openFd, setLock)

data Caps =
  Caps {
    maxRequests :: Maybe Int,
    maxRssMb :: Maybe Int,
    maxLiveMb :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | What a server holds when a request ends: the requests it has answered,
-- its resident set, and the live heap of its last major collection, which is
-- 'Nothing' until one has run or where the RTS keeps no statistics.
data Usage =
  Usage {
    requests :: Int,
    rssKb :: Int,
    liveMb :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | The lock files of a server's slots: @<socket>.lock@ for the first, as a @--jobs 1@ server has always had, and
-- @<socket>.lock.<i>@ for each further one. A directory-mode client holds one of them for its whole request, so a
-- server started with @--jobs N@ creates N and serves N clients at once, and its retirement waits for all of them to
-- come free.
slotLockPaths :: FilePath -> Int -> [FilePath]
slotLockPaths socket n =
  take (max 1 n) ((socket ++ ".lock") : [socket ++ ".lock." ++ show i | i <- [1 :: Int ..]])

-- | Which cap a server has reached, if any, worded for the log line.
capReached :: Caps -> Usage -> Maybe String
capReached Caps {maxRequests, maxRssMb, maxLiveMb} Usage {requests, rssKb, liveMb}
  | Just n <- maxRequests, requests >= n = Just ("request cap " ++ show n)
  | Just mb <- maxLiveMb, Just live <- liveMb, live >= mb = Just ("live heap cap " ++ show mb ++ " MB, live " ++ show live ++ " MB")
  | Just mb <- maxRssMb, rssKb >= mb * 1024 = Just ("memory cap " ++ show mb ++ " MB, VmRSS " ++ show (rssKb `div` 1024) ++ " MB")
  | otherwise = Nothing

-- | The live heap in MB after the last collection, when that collection was a
-- major one. A minor collection's figure covers the nursery alone, so it
-- says nothing about what the server retains.
majorLiveMb :: RTSStats -> Maybe Int
majorLiveMb stats
  | stats.gc.gcdetails_gen > 0 = Just (fromIntegral (stats.gc.gcdetails_live_bytes `div` (1024 * 1024)))
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
    reason :: TVar (Maybe String),
    -- | The live heap of the most recent major collection seen at the end of a
    -- request, kept because the collection just before a check is usually a
    -- minor one.
    lastMajorLiveMb :: TVar (Maybe Int)
  }

newRetirement :: IO Retirement
newRetirement = Retirement <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO Nothing <*> newTVarIO Nothing

-- | Count a request, and after it decide whether the server retires. The
-- request's own response still goes out: the wrapper returns it, and
-- 'awaitRetirement' waits for the client to have read it.
capped :: Caps -> Retirement -> GrpcHandler -> GrpcHandler
capped caps retirement handler =
  GrpcHandler \ commandEnv argv@(RequestArgs args) ->
    bracket_ (count retirement.inFlight 1) (count retirement.inFlight (-1)) do
      out <- handler.run commandEnv argv
      n <- atomically do
        modifyTVar' retirement.requests (+ 1)
        readTVar retirement.requests
      rss <- readVmRssKb
      major <- readMajorLiveMb
      live <- atomically do
        for_ major (writeTVar retirement.lastMajorLiveMb . Just)
        readTVar retirement.lastMajorLiveMb
      inFlight <- readTVarIO retirement.inFlight
      dbg (usageLine n inFlight rss live (requestClass args))
      for_ (capReached caps Usage {requests = n, rssKb = fromMaybe 0 rss, liveMb = live}) \ reached ->
        atomically $ modifyTVar' retirement.reason (maybe (Just (reached ++ ", at " ++ requestClass args)) Just)
      pure out
  where
    count var d = atomically (modifyTVar' var (+ d))

    readMajorLiveMb = do
      enabled <- getRTSStatsEnabled
      if enabled then majorLiveMb <$> getRTSStats else pure Nothing

-- | One line per request on the server's stderr with what the server holds when it ends, so the sizing of a pool can
-- be checked against what its servers retain under concurrent builds: the requests served, the other requests still in
-- flight, the resident set and the live heap of the last major collection.
usageLine :: Int -> Int -> Maybe Int -> Maybe Int -> String -> String
usageLine n inFlight rss live klass =
  unwords ["ghc-worker: usage", "requests", show n, "in_flight", show (inFlight - 1), "rss_mb", maybe "-" (show . (`div` 1024)) rss, "live_mb", maybe "-" show live, klass]

-- | The kind of request that ended past a cap, for the retirement line, so
-- retirements can be counted per request class: a metadata step, or the
-- compile of a unit's module.
requestClass :: [String] -> String
requestClass argv
  | "-M" `elem` argv = "metadata " ++ fromMaybe "?" (after "--unit")
  | otherwise = "compile " ++ fromMaybe "?" (after "--unit") ++ maybe "" (':' :) (after "--module")
  where
    after flag = go argv
      where
        go (x : y : rest)
          | x == flag = Just y
          | otherwise = go (y : rest)
        go _ = Nothing

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
  dbg ("ghc-worker: " ++ reached ++ " reached after " ++ show n ++ " requests; removing " ++ path ++ " and exiting when the requests in flight are answered")
  void $ try @IOException (removeFile socket)
  void $ try @IOException (removeFile (socket <> toOsPath ".cwd"))
  atomically $ readTVar retirement.inFlight >>= check . (== 0)
  locks <- filterM (doesFileExist . toOsPath) (slotLockPaths path maxSlots)
  if null locks then threadDelay 1_000_000 else for_ locks \ lock -> waitForLock lock (400 :: Int)
  dbg ("ghc-worker: retired after " ++ show n ++ " requests")
  where
    path = fromOsPath socket

    -- More slots than any server is started with; the files that exist are the slots.
    maxSlots = 1024

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
