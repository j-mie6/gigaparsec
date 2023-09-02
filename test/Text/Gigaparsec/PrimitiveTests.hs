module Text.Gigaparsec.PrimitiveTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Internal.Test

primitiveTests :: TestTree
primitiveTests = testGroup "primitives"
  [ eofTests
  , pureTests
  ]

eofTests :: TestTree
eofTests = testGroup "eof should"
  [ testCase "fail if input available" $ parse eof "a" @?= Failure
  , testCase "succeed if input ended" $ parse eof "" @?= Success ()
  , testCase "be pure" $ pureParse eof
  ]

pureTests :: TestTree
pureTests = ignoreTestBecause "not implemented" $ testGroup "pure should"
  [ testCase "be pure" $ pureParse (pure 5)
  , testCase "produce the given result" $ parse (pure 5) "" @?= Success 5
  ]
