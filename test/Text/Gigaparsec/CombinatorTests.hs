{-# LANGUAGE BlockArguments #-}
module Text.Gigaparsec.CombinatorTests where

import Test.Tasty
import Test.Tasty.HUnit

--import Control.Monad

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Internal.Test

tests :: TestTree
tests = testGroup "Combinator"
  [ choiceTests
  , optionTests
  , decideTests
  , fromMaybeSTests
  , optionalTests
  , manyNTests
  , skipManyNTests
  , sepByTests
  , sepBy1Tests
  , sepEndByTests
  , sepEndBy1Tests
  , endByTests
  , endBy1Tests
  , manyTillTests
  , someTillTests
  , countTests
  , exactlyTests
  , rangeTests
  , range_Tests
  , countRangeTests
  ]

choiceTests :: TestTree
choiceTests = testGroup "choice should"
  [ testCase "fail if given the empty list" do ensureFails @() (choice []) ""
  , testCase "behave like p for [p]" do
      (choice [char 'a'] ~~ char 'a') ["", "a", "b"]
  , testCase "parse in order" do
      parse (choice [string "a", string "b", string "bc"]) "bcd" @?= Success "b"
  , testCase "fail if none of the parsers succeed" do
      ensureFails (choice [string "a", string "b", string "bc"]) "c"
  ]

optionTests :: TestTree
optionTests = testGroup "option should"
  [ testCase "succeed with Just if p succeeds" do
      parse (option (char 'a')) "a" @?= Success (Just 'a')
  , testCase "succeed with Nothing if p fails withot consumption" do
      parse (option (char 'a')) "b" @?= Success Nothing
  , testCase "fail if p fails with consumption" do
      ensureFails (option (string "ab")) "a"
  ]

decideTests :: TestTree
decideTests = testGroup "decide should"
  [ testCase "succeed for Just" do
      parse (decide (Just <$> char 'a')) "a" @?= Success 'a'
  , testCase "fail for Nothing" do ensureFails @() (decide (pure Nothing)) ""
  , testCase "compose with option to become identity" do
      let id' = decide . option
      (id' (pure 7) ~~ pure 7) [""]
      (id' (char 'a') ~~ char 'a') ["", "a"]
      (id' (string "ab") ~~ string "ab") ["", "a", "ab"]
  ]

fromMaybeSTests :: TestTree
fromMaybeSTests = testGroup "fromMaybeS should"
  [ testCase "succeed for Just" do
      parse (fromMaybeS (pure 'b') (Just <$> char 'a')) "a" @?= Success 'a'
  , testCase "succeed for None" do
      parse (fromMaybeS (pure 'b') (Nothing <$ char 'a')) "a" @?= Success 'b'
  ]

optionalTests :: TestTree
optionalTests = testGroup "optional should"
  [ testCase "succeed if p succeeds" do
      parse (optional (char 'a')) "a" @?= Success ()
  , testCase "also succeed if p fails without consumption" do
      parse (optional (char 'a')) "b" @?= Success ()
  , testCase "fail if p failed with consumption" do
      ensureFails (optional (string "ab")) "a"
  ]

manyNTests :: TestTree
manyNTests = testGroup "manyN should"
  []

skipManyNTests :: TestTree
skipManyNTests = testGroup "skipManyN should"
  []

sepByTests :: TestTree
sepByTests = testGroup "sepBy should"
  []

sepBy1Tests :: TestTree
sepBy1Tests = testGroup "sepBy1 should"
  []

sepEndByTests :: TestTree
sepEndByTests = testGroup "sepEndBy should"
  []

sepEndBy1Tests :: TestTree
sepEndBy1Tests = testGroup "sepEndBy1 should"
  []

endByTests :: TestTree
endByTests = testGroup "endBy should"
  []

endBy1Tests :: TestTree
endBy1Tests = testGroup "endBy1 should"
  []

manyTillTests :: TestTree
manyTillTests = testGroup "manyTill should"
  []

someTillTests :: TestTree
someTillTests = testGroup "someTill should"
  []

countTests :: TestTree
countTests = testGroup "count should"
  []

exactlyTests :: TestTree
exactlyTests = testGroup "exactly should"
  []

rangeTests :: TestTree
rangeTests = testGroup "range should"
  []

range_Tests :: TestTree
range_Tests = testGroup "range_ should"
  []

countRangeTests :: TestTree
countRangeTests = testGroup "countRange should"
  []
