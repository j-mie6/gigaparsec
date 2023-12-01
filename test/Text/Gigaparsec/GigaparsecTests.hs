module Text.Gigaparsec.GigaparsecTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Internal.Test

import Data.Monoid
import Data.Semigroup
import Data.Char (digitToInt)
import Data.Set (singleton, fromList)

tests :: TestTree
tests = testGroup "gigaparsec"
  [ manyMapTests
  , someMapTests
  ]

manyMapTests :: TestTree
manyMapTests = testGroup "manyMap should"
  [ testCase "should treat (: []) as an identity" do
      testParse (manyMap (: []) item) "acbdefh324" @?= Success "acbdefh324"
      testParse (manyMap (: []) item) "" @?= Success ""
  , testCase "should allow for collapsing into any monoid" do
      testParse (manyMap (Sum . digitToInt) digit) "1234" @?= Success 10
      testParse (manyMap (Product . digitToInt) digit) "1234" @?= Success 24
      testParse (manyMap singleton item) "1234aaa43" @?= Success (fromList ['1', '2', '3', '4', 'a'])
  ]

someMapTests :: TestTree
someMapTests = testGroup "someMap should"
  [ testCase "should treat (: []) as an identity" do
      testParse (someMap (: []) item) "abcdef123" @?= Success "abcdef123"
      ensureFails (someMap (: []) item) ""
  , testCase "should allow for collapsing into any semi-group" do
      testParse (someMap Max item) "adehfizs" @?= Success (Max 'z')
      testParse (someMap Min item) "adehfizs" @?= Success (Min 'a')
      testParse (someMap singleton item) "aaabbbccc" @?= Success (fromList ['a', 'b', 'c'])
  ]
