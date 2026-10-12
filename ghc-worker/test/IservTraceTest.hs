module IservTraceTest where

import Data.List (isInfixOf)
import GHCi.Message (Message (..), Msg (..))
import Hedgehog (assert, (===))
import IservTrace (armPrefix, traceLine)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

-- | Stands in for a credential in the eval's environment.
secret :: String
secret = "not-a-real-key-7f3a9c"

-- | What the worker sends to arm an eval: working directory, arguments, environment and output files.
armName :: String
armName =
  armPrefix ++ show armed
  where
    armed :: (FilePath, [String], [(String, String)], Maybe FilePath, Maybe FilePath)
    armed = ("/work", ["--match", "spec"], [("BUILDBUDDY_API_KEY", secret), ("PATH", "/bin")], Just "/tmp/out", Just "/tmp/err")

test_iservTrace :: TestTree
test_iservTrace =
  testGroup "iserv trace" [
    unitTest "an arm's environment never reaches the trace" do
      let line = traceLine (Msg (LookupSymbol armName))
      line === "msg LookupSymbol <arm>"
      assert (not (secret `isInfixOf` line)),
    unitTest "a symbol lookup logs no symbol name" do
      traceLine (Msg (LookupSymbol "base_GHCziBase_id_closure")) === "msg LookupSymbol",
    unitTest "any other message logs its constructor alone" do
      traceLine (Msg Shutdown) === "msg Shutdown"
  ]
