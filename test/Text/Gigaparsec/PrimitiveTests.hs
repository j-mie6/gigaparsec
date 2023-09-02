module Text.Gigaparsec.PrimitiveTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Internal.Test

primitiveTests :: TestTree
primitiveTests = testGroup "primitives"
  [ eofTests
  , pureTests
  , emptyTests
  , apTests
  ]

eofTests :: TestTree
eofTests =
  testGroup "eof should"
    [ testCase "fail if input available" $ parse eof "a" @?= Failure
    , testCase "succeed if input ended" $ parse eof "" @?= Success ()
    , testCase "be pure" $ pureParse eof
    ]

pureTests :: TestTree
pureTests =
  testGroup "pure should"
    [ testCase "be pure" $ pureParse (pure 5)
    , testCase "produce the given result" $ parse (pure 5) "" @?= Success 5
    ]

emptyTests :: TestTree
emptyTests =
  testGroup "empty should"
    [ testCase "be pure" $ pureParse empty
    , testCase "fail unconditionally" $ do
        parse @() empty "" @?= Failure
        parse @() empty "a" @?= Failure
    ]

apTests :: TestTree
apTests = after AllSucceed "/primitives.pure/ || /primitives.empty/" $
  testGroup "(<*>) should"
    [ testCase "be pure if the sub-parsers are" $ pureParse (pure id <*> pure 7)
    , testCase "be impure if either sub-parser is" $ do
        impureParse (consume id <*> pure 7)
        impureParse (pure id <*> consume 7)
        impureParse (consume id <*> consume 7)
    , testCase "sequence the left before the right" $ pureParse (empty <*> consume 7)
    , testCase "be impure if the left is even when right fails" $ impureParse (consume id <*> empty)
    ]
