module CapsTest where

import GhcWorker.Caps (Caps (..), capReached, vmRssKb)
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
  capReached off 100000 (100 * 1024 * 1024) === Nothing
  capReached requests300 299 0 === Nothing
  capReached requests300 300 0 === Just "request cap 300"
  capReached rss6144 1 (6144 * 1024 - 1) === Nothing
  capReached rss6144 1 (6144 * 1024) === Just "memory cap 6144 MB, VmRSS 6144 MB"
  capReached both 300 (7 * 1024 * 1024) === Just "request cap 300"
  capReached both 12 (7 * 1024 * 1024) === Just "memory cap 6144 MB, VmRSS 7168 MB"
  where
    off = Caps {maxRequests = Nothing, maxRssMb = Nothing}
    requests300 = off {maxRequests = Just 300}
    rss6144 = off {maxRssMb = Just 6144}
    both = Caps {maxRequests = Just 300, maxRssMb = Just 6144}

test_caps :: TestTree
test_caps =
  testGroup "retirement caps" [
    unitTest "VmRSS is read from /proc/self/status" test_vmRss,
    unitTest "a cap is reached at its threshold" test_capReached
  ]
