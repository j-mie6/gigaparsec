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
eofTests =
  testGroup "eof should"
    [ testCase "fail if input available" $ ensureFails eof "a"
    , testCase "succeed if input ended" $ parse eof "" @?= Success ()
    , testCase "be pure" $ pureParse eof
    ]

pureTests :: TestTree
pureTests =
  testGroup "pure should"
    [ testCase "be pure" $ pureParse unit
    , testCase "produce the given result" $ parse unit "" @?= Success ()
    ]

emptyTests :: TestTree
emptyTests =
  testGroup "empty should"
    [ testCase "be pure" $ pureParse empty
    , testCase "fail unconditionally" $ do
        ensureFails @Void empty ""
        ensureFails @Void empty "a"
    ]

apTests :: TestTree
apTests = after AllSucceed emptyAndPure $
  testGroup "(<*>) should"
    [ testCase "be pure if the sub-parsers are" $ pureParse (pure id <*> pure 7)
    , testCase "be impure if either sub-parser is" $ do
        impureParse (consume id <*> pure 7)
        impureParse (pure id <*> consume 7)
        impureParse (consume id <*> consume 7)
    , testCase "sequence the left before the right" $ pureParse (empty <*> consume 7)
    , testCase "be impure if the left is even when right fails" $ impureParse (consume id <*> empty)
    ]

orTests :: TestTree
orTests = after AllSucceed emptyPureAndAp $
  testGroup "(<|>) should"
    [ testCase "be pure if the left-hand side is pure and succeeds" $
        pureParse (unit <|> consume ())
    , testCase "be impure if the left-hand side is impure and succeeds" $
        impureParse (consume () <|> empty)
    , testCase "succeed if the left-hand side succeeds" $ do
        parse (unit <|> empty) "" @?= Success ()
        parse (consume () <|> empty) "" @?= Success ()
    , testCase "be pure if the right-hand side succeeds purely" $ pureParse (empty <|> unit)
    , testCase "be impure if the right-hand side succeeds impurely" $
        impureParse (empty <|> consume ())
    , testCase "fail if both sides fail" $ ensureFails @Void (empty <|> empty) ""
    , testCase "be impure if the left-hand side is impure and fails" $
        impureParse (consume () <~> empty <|> empty)
    , testCase "fail if the left-hand side fails impurely" $
        ensureFails (consume () <**> empty <|> unit) ""
    ]

atomicTests :: TestTree
atomicTests = after AllSucceed emptyPureAndAp $
  testGroup "atomic should"
    [ testCase "be pure if the argument is pure" $ do
        pureParse (atomic unit)
        pureParse (atomic empty)
    , testCase "be impure if the argument is impure and succeeds" $
        impureParse (atomic (consume ()))
    , testCase "be pure if the argument fails, even if impure" $
        pureParse (atomic (consume () <**> empty))
    , testCase "not alter failure characteristics of argument" $ do
        parse (atomic (consume ())) "" @?= Success ()
        parse (atomic unit) "" @?= Success ()
        ensureFails @Void (atomic empty) ""
        ensureFails (atomic (consume () <* empty)) ""
    ]

lookAheadTests :: TestTree
lookAheadTests = after AllSucceed emptyPureAndAp $
  testGroup "lookAhead should"
    [ testCase "be pure if the argument is pure" $ do
        pureParse (lookAhead unit)
        pureParse (lookAhead empty)
    , testCase "be pure if the argument is impure and succeeds" $ do
        pureParse (lookAhead (consume ()))
    , testCase "be impure if the argument is impure and fails" $ do
        impureParse (lookAhead (consume () <* empty))
    , testCase "not alter failure characteristics of argument" $ do
        parse (lookAhead (pure 7)) "" @?= Success 7
        parse (lookAhead (consume 14)) "" @?= Success 14
        ensureFails @Void (lookAhead empty) ""
        ensureFails (lookAhead (consume () <* empty)) ""
    ]

notFollowedByTests :: TestTree
notFollowedByTests = after AllSucceed emptyPureAndAp $
  testGroup "notFollowedBy should"
    [ testCase "should always be pure" $ do
        pureParse (notFollowedBy unit)
        pureParse (notFollowedBy empty)
        pureParse (notFollowedBy (consume ()))
        pureParse (notFollowedBy (consume () *> empty))
    , testCase "should succeed if the argument fails" $ do
        parse (notFollowedBy empty) "" @?= Success ()
        parse (notFollowedBy (consume () *> empty)) "" @?= Success ()
    , testCase "should fail if the argument succeeds" $ do
        ensureFails (notFollowedBy unit) ""
        ensureFails (notFollowedBy (consume ())) ""
    ]
