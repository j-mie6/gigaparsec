{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Text.Gigaparsec.ExprTests where
import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Expr
import Text.Gigaparsec.Expr.Subtype

import Text.Gigaparsec.Internal.Test (testParseAll)
import Text.Gigaparsec.Internal.PlainString ()

import Data.Char (digitToInt)

import Text.Gigaparsec.Expr.ChainTests qualified as Chain
import Text.Gigaparsec.Expr.InfixTests qualified as Infix

data Data = Bin Data Data | Un Data | Unit deriving stock (Eq, Show)
data Comp = Less Expr Expr | OfExpr Expr deriving stock (Eq, Show)
data Expr = Add Expr Term | OfTerm Term deriving stock (Eq, Show)
data Term = Mul Factor Term | Mul' Term Factor | OfFactor Factor
  deriving stock (Eq, Show)
data Factor = Neg Factor | OfAtom Atom deriving stock (Eq, Show)
data Atom = Parens Comp | Num Int deriving stock (Eq, Show)

instance Subtype Expr Comp where upcast = OfExpr
instance Subtype Term Expr where upcast = OfTerm
instance Subtype Factor Term where upcast = OfFactor
instance Subtype Atom Factor where upcast = OfAtom

instance Subtype Atom Term where upcast = upcast @Factor . upcast
instance Subtype Atom Expr where upcast = upcast @Term . upcast
instance Subtype Term Comp where upcast = upcast @Expr . upcast

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
      testParseAll expr "1+2*3+4" @?= Success 11
      testParseAll expr "1*2+3*4" @?= Success 14
  , testCase "work for multiple operators at the same level" do
      let expr = precedence' (digitToInt <$> digit) [ops InfixL ["+" $> (+), "-" $> (-)]]
      testParseAll expr "1+2-3+4" @?= Success 4
      testParseAll expr "1-2+3-4" @?= Success (-2)
  , testCase "work for mixed associativity operators" do
      let expr = precedence' (digitToInt <$> digit) [ ops InfixL ["*" $> (*)]
                                                    , ops InfixR ["+" $> (+)]]
      testParseAll expr "1+2*3+4" @?= Success 11
      testParseAll expr "1*2+3*4" @?= Success 14
  , testCase "parse mathematical expressions" do
      let expr = precedence' (digitToInt <$> digit <|> "(" *> expr <* ")")
                             [ ops Prefix ["-" $> negate]
                             , ops InfixL ["/" $> div]
                             , ops InfixR ["*" $> (*)]
                             , ops InfixL ["+" $> (+), "-" $> (-)]]
      testParseAll expr "(2+3)*8" @?= Success 40
      testParseAll expr "-3+4" @?= Success 1
      testParseAll expr "-(3+4)" @?= Success (-7)
      testParseAll expr "(3+-7)*(-2--4)/2" @?= Success (-4)
  , testCase "parse prefix operators mixed with infix operators" do
      let expr = precedence' (digitToInt <$> digit <|> "(" *> expr <* ")")
                             [ ops Prefix ["-" $> negate]
                             , ops InfixL ["-" $> (-)]]
      testParseAll expr "-1" @?= Success (-1)
      testParseAll expr "2-1" @?= Success 1
      testParseAll expr "-2-1" @?= Success (-3)
      testParseAll expr "-(2-1)" @?= Success (-1)
      testParseAll expr "(-0)-1" @?= Success (-1)
  , testCase "be able to parse prefix operators weaker than an infix" do
      let expr = precedence' ("." $> Unit) [ ops InfixL [";" $> Bin]
                                           , ops Prefix ["~" $> Un]]
      testParseAll expr "~.;." @?= Success (Un (Bin Unit Unit))
  , testCase "generalise to sub-typed structures" do
      let expr = precedence $  sops InfixN ["<" $> Less]
                            +< sops InfixL ["+" $> Add]
                            +< sops InfixR ["*" $> Mul]
                            +< sops Prefix ["-" $> Neg]
                            +< Atom (Num . digitToInt <$> digit <|> "(" *> (Parens <$> expr) <* ")")
      testParseAll expr "(7+8)*2+3+6*2" @?=
        Success (upcast (Add (Add (upcast (Mul (upcast (Parens (upcast (Add (upcast (Num 7))
                                                                            (upcast (Num 8))))))
                                               (upcast (Num 2))))
                                  (upcast (Num 3)))
                             (Mul (upcast (Num 6)) (upcast (Num 2)))))
  , testCase "generalise to non-monolithic structures" do
      let expr = precedence $  gops InfixN OfExpr ["<" $> Less]
                            +< gops InfixL OfTerm ["+" $> Add]
                            +< gops InfixR OfFactor ["*" $> Mul]
                            +< gops Prefix OfAtom ["-" $> Neg]
                            +< Atom (Num . digitToInt <$> digit <|> "(" *> (Parens <$> expr) <* ")")
      testParseAll expr "(7+8)*2+3+6*2<4" @?=
        Success (Less (Add (Add (upcast (Mul (upcast (Parens (upcast (Add (upcast (Num 7))
                                                                          (upcast (Num 8))))))
                                               (upcast (Num 2))))
                                  (upcast (Num 3)))
                             (Mul (upcast (Num 6)) (upcast (Num 2))))
                      (upcast (Num 4)))
  , testCase "generalise to non-monolithic structures with more than one chainl1" do
      let expr = precedence $  Atom (Num . digitToInt <$> digit <|> "(" *> (Parens <$> expr) <* ")")
                            >+ gops Prefix OfAtom ["-" $> Neg]
                            >+ gops InfixL OfFactor ["*" $> Mul']
                            >+ gops InfixL OfTerm ["+" $> Add]
                            >+ gops InfixN OfExpr ["<" $> Less]
      testParseAll expr "1*(2+3)" @?=
        Success (upcast (Mul' (upcast (Num 1))
                              (upcast (Parens (upcast (Add (upcast (Num 2))
                                                           (upcast (Num 3))))))))
  ]
