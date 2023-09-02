-- A collection of test helpers
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Gigaparsec.Internal.Test where

import Test.Tasty.HUnit

import Text.Gigaparsec.Internal
import Text.Gigaparsec.Internal.RT

-- don't @ me
deriving stock instance Eq State
deriving stock instance Show State

-- TODO: could we use quick-check to generate states?
-- | Tests to ensure that running the parser on the given string does nothing to the state
pureParseWith :: HasCallStack => Parsec a -> String -> Assertion
pureParseWith (Parsec p) inp = do
  run initSt
  run (initSt { consumed = True })
  run (initSt { line = 10, col = 20 })
  run (initSt { consumed = True, line = 10, col = 20 })
  where initSt = emptyState inp
        run :: State -> Assertion
        run st = runRT (p st (const return) return) @?= st

-- TODO: could we use quick-check to generate inputs?
-- | Tests to ensure that running the parser does nothing to the state
pureParse :: HasCallStack => Parsec a -> Assertion
pureParse p = do
  pureParseWith p ""
  pureParseWith p "a"
  pureParseWith p ":@279"

-- TODO: could we use quick-check to generate states?
-- | Tests to ensure that running the parser on the given string does something to the state
impureParseWith :: HasCallStack => Parsec a -> String -> Assertion
impureParseWith (Parsec p) inp = do
  run initSt
  --run (initSt { consumed = True })
  run (initSt { line = 10, col = 20 })
  --run (initSt { consumed = True, line = 10, col = 20 })
  where initSt = emptyState inp
        run :: State -> Assertion
        run st = assertBool (show st ++ " should be altered") (runRT (p st (const return) return) /= st)

-- TODO: could we use quick-check to generate inputs?
-- | Tests to ensure that running the parser does something to the state
impureParse :: HasCallStack => Parsec a -> Assertion
impureParse p = do
  impureParseWith p ""
  impureParseWith p "a"
  impureParseWith p ":@279"

consume :: a -> Parsec a
consume x = Parsec $ \st good _ -> good x (st { consumed = True})
