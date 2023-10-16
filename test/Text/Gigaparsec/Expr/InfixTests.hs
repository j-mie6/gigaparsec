module Text.Gigaparsec.Expr.InfixTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (string)
import Text.Gigaparsec.Expr.Infix

import Text.Gigaparsec.Internal.Test (parseAll)

import Data.String

instance s ~ String => IsString (Parsec s) where fromString = string

data Expr = Add Int Expr | Sub Expr Int | Num Int deriving stock (Eq, Show)

tests :: TestTree
tests = testGroup "Infix"
  [ infixr1Tests
  , infixl1Tests
  ]

infixr1Tests :: TestTree
infixr1Tests = testGroup "infixr1 should"
  [ testCase "correctly accept the use of a wrapping function" do
      let p = infixr1 Num ("1" $> 1) ("+" $> Add)
      parseAll p "1+1+1" @?= Success (Add 1 (Add 1 (Num 1)))
      parseAll p "1" @?= Success (Num 1)
  ]

infixl1Tests :: TestTree
infixl1Tests = testGroup "infixl1 should"
  [ testCase "correctly accept the use of a wrapping function" do
      let p = infixl1 Num ("1" $> 1) ("-" $> Sub)
      parseAll p "1-1-1" @?= Success (Sub (Sub (Num 1) 1) 1)
      parseAll p "1" @?= Success (Num 1)
  ]
