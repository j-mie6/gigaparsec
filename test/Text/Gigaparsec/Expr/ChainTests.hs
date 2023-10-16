{-# LANGUAGE OverloadedStrings, TypeOperators, BlockArguments, GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Gigaparsec.Expr.ChainTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (string)
import Text.Gigaparsec.Expr.Chain

import Text.Gigaparsec.Internal.Test (parseAll, ensureFails)

import Data.String

instance s ~ String => IsString (Parsec s) where fromString = string

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
      parseAll (chainr1 ("11" $> 1) ("+" $> (+))) "11" @?= Success 1
      ensureFails (chainr1 ("11" $> 1) ("+" $> (+))) "1"
      ensureFails (chainr1 ("11" $> 1) ("+" $> (+))) "2"
  ]

chainrTests :: TestTree
chainrTests = testGroup "chainr should" []

chainl1Tests :: TestTree
chainl1Tests = testGroup "chainl1 should" []

chainlTests :: TestTree
chainlTests = testGroup "chainl should" []
