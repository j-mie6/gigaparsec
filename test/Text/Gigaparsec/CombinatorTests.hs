module Text.Gigaparsec.CombinatorTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad

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
      testParse (choice [string "a", string "b", string "bc"]) "bcd" @?= Success "b"
  , testCase "fail if none of the parsers succeed" do
      ensureFails (choice [string "a", string "b", string "bc"]) "c"
  ]

optionTests :: TestTree
optionTests = testGroup "option should"
  [ testCase "succeed with Just if p succeeds" do
      testParse (option (char 'a')) "a" @?= Success (Just 'a')
  , testCase "succeed with Nothing if p fails withot consumption" do
      testParse (option (char 'a')) "b" @?= Success Nothing
  , testCase "fail if p fails with consumption" do
      ensureFails (option (string "ab")) "a"
  ]

decideTests :: TestTree
decideTests = testGroup "decide should"
  [ testCase "succeed for Just" do
      testParse (decide (Just <$> char 'a')) "a" @?= Success 'a'
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
      testParse (fromMaybeS (pure 'b') (Just <$> char 'a')) "a" @?= Success 'a'
  , testCase "succeed for None" do
      testParse (fromMaybeS (pure 'b') (Nothing <$ char 'a')) "a" @?= Success 'b'
  ]

optionalTests :: TestTree
optionalTests = testGroup "optional should"
  [ testCase "succeed if p succeeds" do
      testParse (optional (char 'a')) "a" @?= Success ()
  , testCase "also succeed if p fails without consumption" do
      testParse (optional (char 'a')) "b" @?= Success ()
  , testCase "fail if p failed with consumption" do
      ensureFails (optional (string "ab")) "a"
  ]

manyNTests :: TestTree
manyNTests = testGroup "manyN should"
  [ testCase "ensure that n are parsed" do
      forM_ [0..10] \n -> do
        testParse (manyN n item) (replicate n 'a') @?= Success (replicate n 'a')
        ensureFails (manyN (n + 1) item) (replicate n 'a')
  , testCase "not care if more are present" do
      forM_ [0..10] \n ->
        testParse (manyN n item) (replicate (n + 1) 'a') @?= Success (replicate (n + 1) 'a')
  ]

skipManyNTests :: TestTree
skipManyNTests = testGroup "skipManyN should"
  [ testCase "ensure that n are parsed" do
      forM_ [0..10] \n -> do
        testParse (skipManyN n item) (replicate n 'a') @?= Success ()
        ensureFails (skipManyN (n + 1) item) (replicate n 'a')
  , testCase "not care if more are present" do
      forM_ [0..10] \n ->
        testParse (skipManyN n item) (replicate (n + 1) 'a') @?= Success ()
  ]

sepByTests :: TestTree
sepByTests = testGroup "sepBy should"
  [ testCase "accept empty input" do
      testParse (sepBy (char 'a') (char 'b')) "" @?= Success []
  , testCase "parse more than 1" do
      testParse (sepBy (char 'a') (char 'b')) "aba" @?= Success ['a', 'a']
  ]

sepBy1Tests :: TestTree
sepBy1Tests = testGroup "sepBy1 should"
  [ testCase "not allow sep at the end of chain" do ensureFails p "ab"
  , testCase "be able to testParse 2 or more p" do
      testParse p "aba" @?= Success ['a', 'a']
      testParse p "ababa" @?= Success ['a', 'a', 'a']
      testParse p "abababa" @?= Success ['a', 'a', 'a', 'a']
  , testCase "require a p" do
      ensureFails p ""
      testParse p "a" @?= Success ['a']
  ]
  where p = sepBy1 (char 'a') (char 'b')

sepEndByTests :: TestTree
sepEndByTests = testGroup "sepEndBy should"
  [ testCase "accept empty input" do
      testParse (sepEndBy (char 'a') (char 'b')) "" @?= Success []
  , testCase "parse more than 1" do
       testParse (sepEndBy (char 'a') (char 'b')) "aba" @?= Success ['a', 'a']
  ]

sepEndBy1Tests :: TestTree
sepEndBy1Tests = testGroup "sepEndBy1 should"
  [ testCase "require a p" do ensureFails p ""
  , testCase "not require sep at end of chain" do testParse p "aa" @?= Success ["aa"]
  , testCase "be able to testParse 2 or more p" do
      testParse p "aabbaa" @?= Success ["aa", "aa"]
      testParse p "aabbaabbaa" @?= Success ["aa", "aa", "aa"]
  , testCase "be able to testParse a final sep" do
      testParse p "aabb" @?= Success ["aa"]
      testParse p "aabbaabb" @?= Success ["aa", "aa"]
      testParse p "aabbaabbaabb" @?= Success ["aa", "aa", "aa"]
  , testCase "fail if p fails after consuming input" do
      ensureFails p "aabab"
  , testCase "fail if sep fails after consuming input" do
      ensureFails p "aab"
  ]
  where p = sepEndBy1 (string "aa") (string "bb")

endByTests :: TestTree
endByTests = testGroup "endBy should"
  [ testCase "accept empty input" do
      testParse (endBy (char 'a') (char 'b')) "" @?= Success []
  , testCase "parse more than 1" do
       testParse (endBy (char 'a') (char 'b')) "abab" @?= Success ['a', 'a']
  ]

endBy1Tests :: TestTree
endBy1Tests = testGroup "endBy1 should"
  [ testCase "require a p" do ensureFails p ""
  , testCase "require a sep at the end of chain" do
      ensureFails p "aa"
      testParse p "aabb" @?= Success ["aa"]
  , testCase "be able to testParse 2 or more p" do
      testParse p "aabbaabb" @?= Success ["aa", "aa"]
      testParse p "aabbaabbaabb" @?= Success ["aa", "aa", "aa"]
  , testCase "fail if p fails after consuming input" do
      ensureFails p "aaba"
  ]
  where p = endBy1 (string "aa") (string "bb")

manyTillTests :: TestTree
manyTillTests = testGroup "manyTill should"
  [ testCase "require an end" do
      ensureFails p "aa"
      testParse p "ab" @?= Success ['a']
  , testCase "parse the end without result" do testParse p "b" @?= Success []
  , testCase "parse p until the end is found" do
      testParse p "aaaaaaaaaab" @?= Success (replicate 10 'a')
      ensureFails (manyTill (string "aa") (char 'b')) "aaab"
  ]
  where p = manyTill (char 'a') (char 'b')

someTillTests :: TestTree
someTillTests = testGroup "someTill should"
  [ testCase "parse at least 1 p" do
      testParse p "ab" @?= Success ['a']
      ensureFails p "a"
      ensureFails p "b"
  ]
  where p = someTill (char 'a') (char 'b')

countTests :: TestTree
countTests = testGroup "count should"
  [ testCase "report how many successful parses occurred" do
      testParse p "" @?= Success 0
      ensureFails q ""
      testParse p "ab" @?= Success 1
      testParse q "ab" @?= Success 1
      testParse p "ababab" @?= Success 3
      testParse q "ababab" @?= Success 3
  , testCase "not allow partial results" do
      ensureFails p "aba"
  ]
  where p = count (string "ab")
        q = count1 (string "ab")

exactlyTests :: TestTree
exactlyTests = testGroup "exactly should"
  [ testCase "should be pure [] for n <= 0" do
      (exactly 0 (char 'a') ~~ pure []) ["", "a"]
      (exactly (-1) (char 'a') ~~ pure []) ["", "a"]
  , testCase "should testParse n times for n > 0" do
      forM_ [0..100] \n ->
        testParse (exactly n (char 'a')) (replicate n 'a') @?= Success (replicate n 'a')
  , testCase "fail if n inputs are not present" do
      ensureFails (exactly 2 (char 'a')) "a"
  ]

rangeTests :: TestTree
rangeTests = testGroup "range should"
  [ testCase "collect results up instead of count" do
      ensureFails p "a"
      testParse p "ab" @?= Success ['a', 'b']
      testParse p "abc" @?= Success ['a', 'b', 'c']
      testParse p "abcd" @?= Success ['a', 'b', 'c', 'd']
      testParse p "abcde" @?= Success ['a', 'b', 'c', 'd', 'e']
      testParse q "abcdef" @?= Success ['a', 'b', 'c', 'd', 'e']
  , testCase "should act as pure [] when range is bad" do
      (range (-1) 3 item ~~ pure []) ["", "a"]
      (range 2 1 item ~~ pure []) ["", "a"]
  ]
  where p = range 2 5 item <* eof
        q = range 2 5 item <* char 'f'

range_Tests :: TestTree
range_Tests = testGroup "range_ should"
  [ testCase "perform a range with no results" do
      ensureFails p "a"
      testParse p "ab" @?= Success ()
      testParse p "abc" @?= Success ()
      testParse p "abcd" @?= Success ()
      testParse p "abcde" @?= Success ()
      testParse q "abcdef" @?= Success ()
  , testCase "should act as unit when range is bad" do
      (range_ (-1) 3 item ~~ unit) ["", "a"]
      (range_ 2 1 item ~~ unit) ["", "a"]
  ]
  where p = range_ 2 5 item <* eof
        q = range_ 2 5 item <* char 'f'

countRangeTests :: TestTree
countRangeTests = testGroup "countRange should"
  [ testCase "count the parses within the range" do
      ensureFails p "ab"
      testParse p "abab" @?= Success 2
      testParse p "ababab" @?= Success 3
      testParse p "abababab" @?= Success 4
      testParse p "ababababab" @?= Success 5
      testParse p "abababababab" @?= Success 5
      ensureFails p "ababababa"
      ensureFails q "ab"
      testParse q "abab" @?= Success 2
      testParse q "ababab" @?= Success 3
      testParse q "abababab" @?= Success 4
      testParse q "ababababab" @?= Success 5
      testParse q "abababababab" @?= Success 5
      testParse q "ababababa" @?= Success 4
  , testCase "should act as unit when range is bad" do
      (countRange (-1) 3 item ~~ pure 0) ["", "a"]
      (countRange 2 1 item ~~ pure 0) ["", "a"]
  ]
  where p = countRange 2 5 (string "ab")
        q = countRange 2 5 (atomic (string "ab"))
