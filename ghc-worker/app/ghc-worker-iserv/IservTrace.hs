{-# LANGUAGE GADTs #-}

-- | What the forking interpreter writes to its trace for each message (P1b-5). Pure, so the rule it keeps is tested:
-- a trace line never carries a message's payload. A lookup's name can be the worker's arm, whose payload is the eval's
-- environment, credentials included (2026-10-12: a trace that logged messages whole wrote BUILDBUDDY_API_KEY to disk).
module IservTrace (armPrefix, disarmName, traceLine) where

import Data.List (isPrefixOf)
import GHCi.Message (Message (..), Msg (..))

-- | The symbol name the worker looks up to arm the next eval, followed by its payload.
armPrefix :: String
armPrefix = "ghc_worker_arm_fork "

-- | The symbol name the worker looks up to disarm.
disarmName :: String
disarmName = "ghc_worker_disarm_fork"

-- | The constructor of a message, never its fields.
traceLine :: Msg -> String
traceLine (Msg msg) =
  "msg " ++ case msg of
    LookupSymbol name
      | armPrefix `isPrefixOf` name -> "LookupSymbol <arm>"
      | otherwise -> "LookupSymbol"
    _ -> takeWhile (/= ' ') (show msg)
