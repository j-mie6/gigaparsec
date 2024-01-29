-- A collection of test helpers
{-# LANGUAGE AllowAmbiguousTypes, RecordWildCards, StandaloneDeriving, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Text.Gigaparsec.Internal.Test where

import Test.Tasty.HUnit

import Text.Gigaparsec.Internal.TestError

import Text.Gigaparsec
import Text.Gigaparsec.Internal
import Text.Gigaparsec.Internal.RT

import Control.Exception (catches, catch, evaluate, Exception, SomeException(..), Handler(..), throwIO)
import Control.Monad (unless, forM_)
import Type.Reflection (typeOf, typeRep)
import Control.DeepSeq (rnf, NFData)

data LiftedState = Lifted State

testParse :: Parsec a -> String -> Result TestError a
testParse = parse @TestError

testParseAll :: Parsec a -> String -> Result TestError a
testParseAll p = testParse (p <* eof)

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
          let st' = runRT (p st (\ !_ s -> return (Lifted s)) (\ _ s -> return (Lifted s)))
          unless (Lifted st == st') $
            assertFailure ("expected no change to internal state\n"
                        ++ "initial state: " ++ show (Lifted st) ++ "\n       became: " ++ show st')

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
          assertBool (show (Lifted st) ++ " should be altered") (st' /= Lifted st)

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
ensureFails p inp = case testParse p inp of
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

notThrow :: NFData a => a -> Assertion
notThrow x = catch (evaluate (rnf x)) $ \(SomeException ex) ->
  assertFailure ("expected no exception but got " ++ show (typeOf ex))

-- TODO: we want result/error comparison later down the line
(~~) :: HasCallStack => Parsec a -> Parsec a -> [String] -> Assertion
(p ~~ q) inps =
  forM_ inps $ \inp -> do
    let st = emptyState inp
        pSt = parseState p st
        qSt = parseState q st
    unless (pSt == qSt) $
      assertFailure ("expected both parsers have the same effect on the state"
                  ++ "\ninitial state: " ++ show (Lifted st)
                  ++ "\n          got: " ++ show pSt
                  ++ "\n     expected: " ++ show qSt)

parseState :: Parsec a -> State -> LiftedState
parseState (Parsec p) st = runRT (p st (\ !_ st' -> return (Lifted st')) (\ _ st' -> return (Lifted st')))

deriving anyclass instance (NFData e, NFData a) => NFData (Result e a)

-- don't @ me
instance Eq LiftedState where
  (==) :: LiftedState -> LiftedState -> Bool
  Lifted (State input1 consumed1 line1 col1 _hintValidOffset1 _hints1 _debugLevel1) ==
    Lifted (State input2 consumed2 line2 col2 _hintValidOffset2 _hints2 _debugLevel2) =
       consumed1 == consumed2 && line1 == line2 && col1 == col2 && input1 == input2
    -- this throws off a whole bunch of tests, understandably
    -- && hintValidOffset1 == hintValidOffset2 && hints1 == hints2
instance Show LiftedState where
  showsPrec :: Int -> LiftedState -> ShowS
  showsPrec p (Lifted State{..}) = showParen (p > 10) $ showString "State { input = "
                                                      . shows input
                                                      . showString ", consumed = "
                                                      . shows consumed
                                                      . showString ", line = "
                                                      . shows line
                                                      . showString ", col = "
                                                      . shows col
                                                      . showString ", hintsValidOffset = "
                                                      . shows hintsValidOffset
                                                      -- . showString ", hints = "
                                                      -- . shows hints
                                                      . showChar '}'
