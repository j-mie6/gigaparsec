{-# LANGUAGE OverloadedLists #-}
module Text.Gigaparsec.CharTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Char
import Data.Map.Strict qualified as Map

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Internal.Require
import Text.Gigaparsec.Internal.Test

tests :: TestTree
tests = testGroup "Char"
  [ satisfyTests
  , charTests
  , stringTests
  , satisfyMapTests
  , oneOfTests
  , noneOfTests
  , stringsTests
  , trieTests
  ]

satisfyTests :: TestTree
satisfyTests = testGroup "satisfy should"
  [ testCase "be pure if it cannot read" do
      pureParse (satisfy (const False))
      pureParseWith (satisfy (== 'a')) ""
      pureParseWith (satisfy (== 'a')) "b"
  , testCase "be impure otherwise" do impureParseWith (satisfy (== 'a')) "a"
  , testCase "return the parsed character" do
      testParse item "a" @?= Success 'a'
      testParse item "ba" @?= Success 'b'
      testParse item "\NUL" @?= Success '\NUL'
  , testCase "fail otherwise" do
      ensureFails (satisfy (== 'b')) ""
      ensureFails item ""
      ensureFails (satisfy (== 'b')) "a"
  ]

charTests :: TestTree
charTests = testGroup "char should"
  [ testCase "be pure if it cannot read" do
      pureParseWith (char 'a') ""
      pureParseWith (char 'a') "b"
  , testCase "be impure otherwise" do impureParseWith (char 'a') "a"
  , testCase "return the parsed character" do testParse (char 'a') "a" @?= Success 'a'
  , testCase "fail otherwise" do
      ensureFails (char 'a') ""
      ensureFails (char 'a') "b"
  ]

stringTests :: TestTree
stringTests = testGroup "string should"
  [ testCase "reject the empty string" (throws @RequirementUnsatisfied (testParse (string "") "")) -- don't ask why `string ""` doesn't work all the time
  , testCase "be pure if it cannot read at all" do
      pureParseWith (string "abc") ""
      pureParseWith (string "abc") "123"
  , testCase "be impure if there is a partial read" do impureParseWith (string "abc") "abd"
  , testCase "be impure if full read" do impureParseWith (string "abc") "abc"
  , testCase "return the parsed string" do testParse (string "123") "123" @?= Success "123"
  , testCase "fail otherwise" do
      ensureFails (string "123") "124"
      ensureFails (string "123") "12"
  ]

satisfyMapTests :: TestTree
satisfyMapTests = testGroup "satisfyMap should"
  [ testCase "fail when invalid" do
      ensureFails p ""
      ensureFails p "a"
  , testCase "succeed performing the mapping otherwise" do
      testParse p "4" @?= Success 4
      testParse p "9" @?= Success 9
  ]
  where p = satisfyMap (\c -> digitToInt c <$ guard (isDigit c))

oneOfTests :: TestTree
oneOfTests = testGroup "oneOf should"
  [ testCase "fail when given no characters" do
      pureParse p
      ensureFails p ""
      ensureFails p "a"
  , testCase "act like character given one character" do
      pureParseWith q ""
      pureParseWith q "b"
      impureParseWith q "a"
      testParse q "a" @?= Success 'a'
      ensureFails q ""
      ensureFails q "b"
  , testCase "parse within a contiguous range" do
      pureParseWith r ""
      pureParseWith r "a"
      forM_ @[] ['0'..'9'] $ \c -> do
        impureParseWith r (pure c)
        testParse r (pure c) @?= Success c
      ensureFails r "a"
      ensureFails r "\NUL"
      ensureFails r ":"
      ensureFails r "/"
  , testCase "parse any other sets" do
      pureParseWith s ""
      pureParseWith s "a"
      forM_ @[] ['.', ';', ',', ':'] $ \c -> do
        impureParseWith s (pure c)
        testParse s (pure c) @?= Success c
      ensureFails s "a"
      ensureFails s "\NUL"
  ]
  where p = oneOf []
        q = oneOf ['a']
        r = oneOf ['0' .. '9']
        s = oneOf ['.', ';', ',', ':']

noneOfTests :: TestTree
noneOfTests = testGroup "oneOf should"
  [ testCase "act like item when given no characters" do
      pureParseWith p ""
      ensureFails p ""
      testParse p "a" @?= Success 'a'
      testParse p "\ACK" @?= Success '\ACK'
  , testCase "accept all but a specific character" do
      pureParseWith q ""
      pureParseWith q "a"
      impureParseWith q "b"
      testParse q "5" @?= Success '5'
      ensureFails q ""
      ensureFails q "a"
  , testCase "parse within a contiguous range" do
      pureParseWith r ""
      impureParseWith r "a"
      forM_ @[] ['0'..'9'] $ \c -> do
        pureParseWith r (pure c)
        ensureFails r (pure c)
      testParse r "a" @?= Success 'a'
      testParse r "\NUL" @?= Success '\NUL'
      testParse r ":" @?= Success ':'
      testParse r "/" @?= Success '/'
  , testCase "parse any other sets" do
      pureParseWith s ""
      impureParseWith s "a"
      forM_ @[] ['.', ';', ',', ':'] $ \c -> do
        pureParseWith s (pure c)
        ensureFails s (pure c)
      testParse s "a" @?= Success 'a'
      testParse s "\NUL" @?= Success '\NUL'
  ]
  where p = noneOf []
        q = noneOf ['a']
        r = noneOf ['0' .. '9']
        s = noneOf ['.', ';', ',', ':']

stringsTests :: TestTree
stringsTests = testGroup "strings should"
  [ testCase "reject any empty strings" do throws @RequirementUnsatisfied (strings ["abc", "323", ""])
  , testCase "have longest match behaviour" do
      testParse p "hello" @?= Success "hello"
      testParse p "hell" @?= Success "hell"
      testParse p "he" @?= Success "h"
      testParse p "123" @?= Success "123"
      testParse p "124" @?= Success "1"
  , testCase "reject anything outside of the set" do
      ensureFails p "543"
      ensureFails p "good"
  ]
  where p = strings ["hell", "hello", "h", "123", "1"]

trieTests :: TestTree
trieTests = testGroup "trie should"
  [ testCase "reject any empty strings" do throws @RequirementUnsatisfied (trie' ["" --> unit])
  , testCase "have longest match behaviour" do
      testParse p "hello" @?= Success "hello"
      testParse p "hell" @?= Success "hell"
      testParse p "h" @?= Success "h"
      testParse p "he" @?= Success "h"
      testParse p "hi" @?= Success "hi"
      testParse p "good" @?= Success "good"
      testParse p "goodby" @?= Success "good"
      testParse p "goodbye" @?= Success "goodbye"
  , testCase "reject anything outside of the set" do
      ensureFails p "543"
      ensureFails p "god"
  ]
  where p = trie'
              [ "h" --> atomic (trie' [ "ell" --> trie' ["o" --> pure "hello"]
                                              <|> pure "hell"
                                      , "i"   --> pure "hi"
                                      ])
                    <|> pure "h"
              , "good" --> atomic (trie' ["bye" --> pure "goodbye"])
                       <|> pure "good"
              ]
        infix 0 -->
        (-->) = (,)
        trie' = trie . Map.fromList
