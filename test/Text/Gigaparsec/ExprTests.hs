module Text.Gigaparsec.ExprTests where

import Test.Tasty
--import Test.Tasty.HUnit

--import Text.Gigaparsec
--import Text.Gigaparsec.Expr

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
  []
