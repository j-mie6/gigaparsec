{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, right identity" #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Use <$>" #-}
module Text.Gigaparsec.PrimitiveTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Internal.Test

import Data.Void

emptyAndPure, emptyPureAndAp :: String
emptyAndPure = "/primitives.pure/ || /primitives.empty/"
emptyPureAndAp = emptyAndPure ++ " || /primitives.(<*>)/"

tests :: TestTree
tests = testGroup "primitives"
  [ eofTests
  , pureTests
  , emptyTests
  , apTests
  , orTests
  , atomicTests
  , lookAheadTests
  , notFollowedByTests
  -- TODO: _branch, monad, semigroup?
  ]

eofTests :: TestTree
eofTests = testGroup "eof should"
  [ testCase "fail if input available" do ensureFails eof "a"
  , testCase "succeed if input ended" do testParse eof "" @?= Success ()
  , testCase "be pure" do pureParse eof
  ]

pureTests :: TestTree
pureTests = testGroup "pure should"
  [ testCase "be pure" do pureParse unit
  , testCase "produce the given result" do testParse unit "" @?= Success ()
  ]

emptyTests :: TestTree
emptyTests = testGroup "empty should"
  [ testCase "be pure" do pureParse empty
  , testCase "fail unconditionally" do
      ensureFails @Void empty ""
      ensureFails @Void empty "a"
  ]

apTests :: TestTree
apTests = after AllSucceed emptyAndPure $
  testGroup "(<*>) should"
    [ testCase "be pure if the sub-parsers are" do pureParse (pure id <*> pure 7)
    , testCase "be impure if either sub-parser is" do
        impureParse (consume id <*> pure 7)
        impureParse (pure id <*> consume 7)
        impureParse (consume id <*> consume 7)
    , testCase "sequence the left before the right" do pureParse (empty <*> consume 7)
    , testCase "be impure if the left is even when right fails" do impureParse (consume id <*> empty)
    ]

orTests :: TestTree
orTests = after AllSucceed emptyPureAndAp $
  testGroup "(<|>) should"
    [ testCase "be pure if the left-hand side is pure and succeeds" do
        pureParse (unit <|> consume ())
    , testCase "be impure if the left-hand side is impure and succeeds" do
        impureParse (consume () <|> empty)
    , testCase "succeed if the left-hand side succeeds" do
        testParse (unit <|> empty) "" @?= Success ()
        testParse (consume () <|> empty) "" @?= Success ()
    , testCase "be pure if the right-hand side succeeds purely" do
        pureParse (empty <|> unit)
    , testCase "be impure if the right-hand side succeeds impurely" do
        impureParse (empty <|> consume ())
    , testCase "fail if both sides fail" do ensureFails @Void (empty <|> empty) ""
    , testCase "be impure if the left-hand side is impure and fails" do
        impureParse (consume () <~> empty <|> empty)
    , testCase "fail if the left-hand side fails impurely" do
        ensureFails (consume () <**> empty <|> unit) ""
    ]

atomicTests :: TestTree
atomicTests = after AllSucceed emptyPureAndAp $
  testGroup "atomic should"
    [ testCase "be pure if the argument is pure" do
        pureParse (atomic unit)
        pureParse (atomic empty)
    , testCase "be impure if the argument is impure and succeeds" do
        impureParse (atomic (consume ()))
    , testCase "be pure if the argument fails, even if impure" do
        pureParse (atomic (consume () <**> empty))
    , testCase "not alter failure characteristics of argument" do
        testParse (atomic (consume ())) "" @?= Success ()
        testParse (atomic unit) "" @?= Success ()
        ensureFails @Void (atomic empty) ""
        ensureFails (atomic (consume () <* empty)) ""
    ]

lookAheadTests :: TestTree
lookAheadTests = after AllSucceed emptyPureAndAp $
  testGroup "lookAhead should"
    [ testCase "be pure if the argument is pure" do
        pureParse (lookAhead unit)
        pureParse (lookAhead empty)
    , testCase "be pure if the argument is impure and succeeds" do
        pureParse (lookAhead (consume ()))
    , testCase "be impure if the argument is impure and fails" do
        impureParse (lookAhead (consume () <* empty))
    , testCase "not alter failure characteristics of argument" do
        testParse (lookAhead (pure 7)) "" @?= Success 7
        testParse (lookAhead (consume 14)) "" @?= Success 14
        ensureFails @Void (lookAhead empty) ""
        ensureFails (lookAhead (consume () <* empty)) ""
    ]

notFollowedByTests :: TestTree
notFollowedByTests = after AllSucceed emptyPureAndAp $
  testGroup "notFollowedBy should"
    [ testCase "should always be pure" do
        pureParse (notFollowedBy unit)
        pureParse (notFollowedBy empty)
        pureParse (notFollowedBy (consume ()))
        pureParse (notFollowedBy (consume () *> empty))
    , testCase "should succeed if the argument fails" do
        testParse (notFollowedBy empty) "" @?= Success ()
        testParse (notFollowedBy (consume () *> empty)) "" @?= Success ()
    , testCase "should fail if the argument succeeds" do
        ensureFails (notFollowedBy unit) ""
        ensureFails (notFollowedBy (consume ())) ""
    ]
