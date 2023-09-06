module Main (main) where

import Test.Tasty

import Text.Gigaparsec.PrimitiveTests qualified as Primitive
import Text.Gigaparsec.CharTests qualified as Char

main :: IO ()
main = defaultMain $ testGroup "gigaparsec"
  [ Primitive.tests
  , Char.tests
  ]
