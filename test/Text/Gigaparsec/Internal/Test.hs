-- A collection of test helpers
{-# LANGUAGE StandaloneDeriving, AllowAmbiguousTypes #-}
module Text.Gigaparsec.Internal.Test where

import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Internal
import Text.Gigaparsec.Internal.RT

import Control.Exception (catches, evaluate, Exception, SomeException(..), Handler(..), throwIO)
import Control.Monad (unless, forM_)
import Type.Reflection (typeOf, typeRep)

-- don't @ me
deriving stock instance Eq State
deriving stock instance Show State

parseAll :: Parsec a -> String -> Result a
parseAll p = parse (p <* eof)

-- TODO: could we use quick-check to generate states?
-- | Tests to ensure that running the parser on the given string does nothing to the state
pureParseWith :: HasCallStack => Parsec a -> String -> Assertion
pureParseWith (Parsec p) inp = do
  run initSt
  run (initSt { consumed = 1 })
  run (initSt { line = 10, col = 20 })
  run (initSt { consumed = 200, line = 10, col = 20 })
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
impureParseWith p inp = do
  run initSt
  --run (initSt { consumed = True })
  run (initSt { line = 10, col = 20 })
  --run (initSt { consumed = True, line = 10, col = 20 })
  where initSt = emptyState inp
        run :: State -> Assertion
        run st = do
          let st' = parseState p st
          assertBool (show st ++ " should be altered") (st' /= st)

-- TODO: could we use quick-check to generate inputs?
-- | Tests to ensure that running the parser does something to the state
impureParse :: HasCallStack => Parsec a -> Assertion
impureParse p = do
  impureParseWith p ""
  impureParseWith p "a"
  impureParseWith p ":@279"

consume :: a -> Parsec a
consume x = Parsec $ \st good _ -> good x (st { consumed = consumed st + 1})

ensureFails :: (Show a, HasCallStack) => Parsec a -> String -> Assertion
ensureFails p inp = case parse p inp of
  Failure{} -> return ()
  Success x -> assertFailure ("parser must fail, but produced: " ++ show x)

throws :: forall e a. (HasCallStack, Exception e) => a -> Assertion
throws x = do
  catches (evaluate x >> assertFailure ("expected: " ++ show (typeRep @e)))
    [ Handler $ \ ((!_) :: e) -> return ()
    , Handler $ \ (ex :: HUnitFailure) -> throwIO ex
    , Handler $ \ (SomeException ex) -> assertFailure ("expected: " ++ show (typeRep @e) ++ "\n"
                                                    ++ " but got: " ++ show (typeOf ex))
    ]

-- TODO: we want result/error comparison later down the line
(~~) :: HasCallStack => Parsec a -> Parsec a -> [String] -> Assertion
(p ~~ q) inps =
  forM_ inps $ \inp -> do
    let st = emptyState inp
        pSt = parseState p st
        qSt = parseState q st
    unless (pSt == qSt) $
      assertFailure ("expected both parsers have the same effect on the state"
                  ++ "\ninitial state: " ++ show st
                  ++ "\n          got: " ++ show pSt
                  ++ "\n     expected: " ++ show qSt)

parseState :: Parsec a -> State -> State
parseState (Parsec p) st = runRT (p st (\ !_ -> return) return)
