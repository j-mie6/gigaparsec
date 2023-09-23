module Main (main) where

import Test.Tasty

import Text.Gigaparsec.PrimitiveTests qualified as Primitive
import Text.Gigaparsec.CharTests qualified as Char
import Text.Gigaparsec.CombinatorTests qualified as Combinator

main :: IO ()
main = defaultMain $ testGroup "gigaparsec"
  [ Primitive.tests
  , Char.tests
  , Combinator.tests
  ]
