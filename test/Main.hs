module Main (main) where

import Test.Tasty

import Text.Gigaparsec.PrimitiveTests qualified as Primitive
import Text.Gigaparsec.CharTests qualified as Char
import Text.Gigaparsec.CombinatorTests qualified as Combinator
import Text.Gigaparsec.DebugTests qualified as Debug
import Text.Gigaparsec.ExprTests qualified as Expr
import Text.Gigaparsec.ErrorsTests qualified as Errors
import Text.Gigaparsec.TokenTests qualified as Token

main :: IO ()
main = defaultMain $ testGroup "gigaparsec"
  [ Primitive.tests
  , Char.tests
  , Combinator.tests
  , Expr.tests
  , Errors.tests
  , Token.tests
  , Debug.tests
  ]
