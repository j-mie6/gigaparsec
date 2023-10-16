module Text.Gigaparsec.Expr.ChainTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Expr.Chain

import Text.Gigaparsec.Internal.Test (parseAll, ensureFails)
import Text.Gigaparsec.Internal.PlainString ()

import Data.Char (digitToInt)

tests :: TestTree
tests = testGroup "Chain"
  [ postfixTests
  , postfix1Tests
  , prefixTests
  , prefix1Tests
  , chainr1Tests
  , chainrTests
  , chainl1Tests
  , chainlTests
  ]

postfixTests :: TestTree
postfixTests = testGroup "postfix should"
  [ testCase "require an initial value" do
      parseAll (postfix ("1" $> 1) ("+" $> succ)) "1" @?= Success 1
  , testCase "parse all operators that follow" do
      parseAll (postfix ("1" $> 1) ("+" $> succ)) "1++++++++++++++" @?= Success 15
  , testCase "fail if an operator fails after consuming input" do
      ensureFails (postfix ("1" $> 1) ("++" $> succ)) "1+++++++++++++++"
  ]

postfix1Tests :: TestTree
postfix1Tests = testGroup "postfix1 should"
  [ testCase "require an initial value" do
      ensureFails (postfix1 id ("1" $> 1) ("+" $> succ)) "1"
      parseAll (postfix1 id ("1" $> 1) ("+" $> succ)) "1+" @?= Success 2
  , testCase "parse all operators that follow" do
      parseAll (postfix1 id ("1" $> 1) ("+" $> succ)) "1++++++++++++++" @?= Success 15
  , testCase "fail if an operator fails after consuming input" do
      ensureFails (postfix1 id ("1" $> 1) ("++" $> succ)) "1+++++++++++++++"
  ]

prefixTests :: TestTree
prefixTests = testGroup "prefix should"
   [ testCase "require an initial value" do
      parseAll (prefix ("+" $> succ) ("1" $> 1)) "1" @?= Success 1
  , testCase "parse all operators that follow" do
      parseAll (prefix ("+" $> succ) ("1" $> 1)) "++++++++++++++1" @?= Success 15
  , testCase "fail if an operator fails after consuming input" do
      ensureFails (prefix ("++" $> succ) ("1" $> 1)) "+++++++++++++++1"
  ]

prefix1Tests :: TestTree
prefix1Tests = testGroup "prefix1 should"
  [ testCase "require an initial value" do
      ensureFails (prefix1 id ("+" $> succ) ("1" $> 1)) "1"
      parseAll (prefix1 id ("+" $> succ) ("1" $> 1)) "+1" @?= Success 2
  , testCase "parse all operators that follow" do
      parseAll (prefix1 id ("+" $> succ) ("1" $> 1)) "++++++++++++++1" @?= Success 15
  , testCase "fail if an operator fails after consuming input" do
      ensureFails (prefix1 id ("++" $> succ) ("1" $> 1)) "+++++++++++++++1"
  ]

chainr1Tests :: TestTree
chainr1Tests = testGroup "chainr1 should"
  [ testCase "require an initial value" do
      let p = chainr1 ("11" $> 1) ("+" $> (+))
      parseAll p "11" @?= Success 1
      ensureFails p "1"
      ensureFails p "2"
  , testCase "parse all operators and values that follow" do
      parseAll (chainr1 ("11" $> 1) ("+" $> (+))) "11+11+11+11+11" @?= Success 5
  , testCase "apply the functions with the correct associativity" do
      parseAll (chainr1 (digitToInt <$> digit) ("%" $> mod)) "6%5%2%7" @?= Success 0
  , testCase "fail if an operator or p fails after consuming input" do
      let p = chainr1 ("11" $> 1) ("++" $> (+))
      ensureFails p "11+11+11+11+11"
      ensureFails p "11++11++11++1++11"
  ]

chainrTests :: TestTree
chainrTests = testGroup "chainr should"
  [ testCase "allow for no initial value" do
      let p = chainr ("11" $> 1) ("+" $> (+)) 0
      parseAll p "" @?= Success 0
      ensureFails p "1"
  ]

chainl1Tests :: TestTree
chainl1Tests = testGroup "chainl1 should"
  [ testCase "require an initial value" do
      let p = chainl1 ("11" $> 1) ("+" $> (+))
      parseAll p "11" @?= Success 1
      ensureFails p "1"
      ensureFails p "2"
  , testCase "parse all operators and values that follow" do
      parseAll (chainl1 ("11" $> 1) ("+" $> (+))) "11+11+11+11+11" @?= Success 5
  , testCase "apply the functions with the correct associativity" do
      parseAll (chainl1 (digitToInt <$> digit) ("%" $> mod)) "6%5%2%7" @?= Success 1
  , testCase "fail if an operator or p fails after consuming input" do
      let p = chainl1 ("11" $> 1) ("++" $> (+))
      ensureFails p "11+11+11+11+11"
      ensureFails p "11++11++11++1++11"
  ]

chainlTests :: TestTree
chainlTests = testGroup "chainl should"
  [ testCase "allow for no initial value" do
      let p = chainl ("11" $> 1) ("+" $> (+)) 0
      parseAll p "" @?= Success 0
      ensureFails p "1"
      parse p "2" @?= Success 0
  ]
