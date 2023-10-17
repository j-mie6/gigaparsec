{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Text.Gigaparsec.ExprTests where
import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Expr
import Text.Gigaparsec.Expr.Subtype

import Text.Gigaparsec.Internal.Test (parseAll)
import Text.Gigaparsec.Internal.PlainString ()

import Data.Char (digitToInt)

import Text.Gigaparsec.Expr.ChainTests qualified as Chain
import Text.Gigaparsec.Expr.InfixTests qualified as Infix

data Data = Bin Data Data | Un Data | Unit deriving stock (Eq, Show)
data Comp = Less Expr Expr | OfExpr Expr deriving stock (Eq, Show)
data Expr = Add Expr Term | OfTerm Term deriving stock (Eq, Show)
data Term = Mul Factor Term | OfFactor Factor deriving stock (Eq, Show)
data Factor = Neg Factor | OfAtom Atom deriving stock (Eq, Show)
data Atom = Parens Comp | Num Int deriving stock (Eq, Show)

instance Subtype Expr Comp where upcast = OfExpr
instance Subtype Term Expr where upcast = OfTerm
instance Subtype Factor Term where upcast = OfFactor
instance Subtype Atom Factor where upcast = OfAtom

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
  , testCase "work for mixed associativity operators" do
      let expr = precedence' (digitToInt <$> digit) [ ops InfixL ["*" $> (*)]
                                                    , ops InfixR ["+" $> (+)]]
      parseAll expr "1+2*3+4" @?= Success 11
      parseAll expr "1*2+3*4" @?= Success 14
  , testCase "parse mathematical expressions" do
      let expr = precedence' (digitToInt <$> digit <|> "(" *> expr <* ")")
                             [ ops Prefix ["-" $> negate]
                             , ops InfixL ["/" $> div]
                             , ops InfixR ["*" $> (*)]
                             , ops InfixL ["+" $> (+), "-" $> (-)]]
      parseAll expr "(2+3)*8" @?= Success 40
      parseAll expr "-3+4" @?= Success 1
      parseAll expr "-(3+4)" @?= Success (-7)
      parseAll expr "(3+-7)*(-2--4)/2" @?= Success (-4)
  , testCase "parse prefix operators mixed with infix operators" do
      let expr = precedence' (digitToInt <$> digit <|> "(" *> expr <* ")")
                             [ ops Prefix ["-" $> negate]
                             , ops InfixL ["-" $> (-)]]
      parseAll expr "-1" @?= Success (-1)
      parseAll expr "2-1" @?= Success 1
      parseAll expr "-2-1" @?= Success (-3)
      parseAll expr "-(2-1)" @?= Success (-1)
      parseAll expr "(-0)-1" @?= Success (-1)
  , testCase "be able to parse prefix operators weaker than an infix" do
      let expr = precedence' ("." $> Unit) [ ops InfixL [";" $> Bin]
                                           , ops Prefix ["~" $> Un]]
      parseAll expr "~.;." @?= Success (Un (Bin Unit Unit))
  ]
