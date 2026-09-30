module CapsTest where

import GhcWorker.Caps (Caps (..), Usage (..), capReached, vmRssKb)
import Hedgehog (TestT, (===))
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

-- | The lines around @VmRSS@ in a Linux @/proc/self/status@.
status :: String
status =
  unlines [
    "Name:\tghc-worker",
    "VmPeak:\t 9375476 kB",
    "VmSize:\t 9375476 kB",
    "VmHWM:\t 6553600 kB",
    "VmRSS:\t 6553600 kB",
    "RssAnon:\t 6500000 kB",
    "Threads:\t37"
  ]

test_vmRss :: TestT IO ()
test_vmRss = do
  vmRssKb status === Just 6553600
  vmRssKb "Name:\tghc-worker\nThreads:\t37\n" === Nothing
  vmRssKb "" === Nothing

test_capReached :: TestT IO ()
test_capReached = do
  capReached off (usage 100000 (100 * 1024 * 1024) (Just 90000)) === Nothing
  capReached requests300 (usage 299 0 Nothing) === Nothing
  capReached requests300 (usage 300 0 Nothing) === Just "request cap 300"
  capReached rss6144 (usage 1 (6144 * 1024 - 1) Nothing) === Nothing
  capReached rss6144 (usage 1 (6144 * 1024) Nothing) === Just "memory cap 6144 MB, VmRSS 6144 MB"
  capReached both (usage 300 (7 * 1024 * 1024) Nothing) === Just "request cap 300"
  capReached both (usage 12 (7 * 1024 * 1024) Nothing) === Just "memory cap 6144 MB, VmRSS 7168 MB"
  where
    usage requests rssKb liveMb = Usage {requests, rssKb, liveMb}
    off = Caps {maxRequests = Nothing, maxRssMb = Nothing, maxLiveMb = Nothing}
    requests300 = off {maxRequests = Just 300}
    rss6144 = off {maxRssMb = Just 6144}
    both = off {maxRequests = Just 300, maxRssMb = Just 6144}

-- | A heavy compile leaves the resident set at its peak, 22.9 GB for mwb's
-- heaviest module, while what the server keeps is a few GB. The live-heap cap
-- lets such a server go on serving and still retires one whose kept state
-- grew; with no major collection yet it cannot decide and does not retire.
test_liveCap :: TestT IO ()
test_liveCap = do
  capReached live8192 (usage (23 * 1024 * 1024) (Just 3500)) === Nothing
  capReached live8192 (usage (23 * 1024 * 1024) (Just 8192)) === Just "live heap cap 8192 MB, live 8192 MB"
  capReached live8192 (usage (23 * 1024 * 1024) Nothing) === Nothing
  where
    usage rssKb liveMb = Usage {requests = 5, rssKb, liveMb}
    live8192 = Caps {maxRequests = Nothing, maxRssMb = Nothing, maxLiveMb = Just 8192}

test_caps :: TestTree
test_caps =
  testGroup "retirement caps" [
    unitTest "VmRSS is read from /proc/self/status" test_vmRss,
    unitTest "a cap is reached at its threshold" test_capReached,
    unitTest "a compile's peak does not reach the live-heap cap" test_liveCap
  ]
