module Text.Gigaparsec.ExprTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Expr

import Text.Gigaparsec.Internal.Test (parseAll)
import Text.Gigaparsec.Internal.PlainString ()

import Data.Char (digitToInt)

import Text.Gigaparsec.Expr.ChainTests qualified as Chain
import Text.Gigaparsec.Expr.InfixTests qualified as Infix

tests :: TestTree
tests = testGroup "Expr"
  [ Chain.tests
  , Infix.tests
  , precTests
  ]

precTests :: TestTree
precTests = testGroup "precedence should"
  [ testCase "result in correct precedence" do
      let expr = precedence' (digitToInt <$> digit) [ ops InfixL ["*" $> (*)]
                                                    , ops InfixL ["+" $> (+)]]
      parseAll expr "1+2*3+4" @?= Success 11
      parseAll expr "1*2+3*4" @?= Success 14
  , testCase "work for multiple operators at the same level" do
      let expr = precedence' (digitToInt <$> digit) [ops InfixL ["+" $> (+), "-" $> (-)]]
      parseAll expr "1+2-3+4" @?= Success 4
      parseAll expr "1-2+3-4" @?= Success (-2)
  ]
