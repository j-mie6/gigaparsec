-- A collection of test helpers
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Gigaparsec.Internal.Test where

import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Internal
import Text.Gigaparsec.Internal.RT

import Control.Monad (unless)

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
        run st = do
          let st' = runRT (p st (\ !_ -> return) return)
          unless (st == st') $
            assertFailure ("expected no change to internal state\n"
                        ++ "initial state: " ++ show st ++ "\n       became: " ++ show st')

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
        run st = do
          let st' = runRT (p st (\ !_ -> return) return)
          assertBool (show st ++ " should be altered") (st' /= st)

-- TODO: could we use quick-check to generate inputs?
-- | Tests to ensure that running the parser does something to the state
impureParse :: HasCallStack => Parsec a -> Assertion
impureParse p = do
  impureParseWith p ""
  impureParseWith p "a"
  impureParseWith p ":@279"

consume :: a -> Parsec a
consume x = Parsec $ \st good _ -> good x (st { consumed = True})

ensureFails :: (Show a, HasCallStack) => Parsec a -> String -> Assertion
ensureFails p inp = case parse p inp of
  Failure{} -> return ()
  Success x -> assertFailure ("parser must fail, but produced: " ++ show x)
