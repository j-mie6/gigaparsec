module Main (main) where

import Test.Tasty

import Text.Gigaparsec.PrimitiveTests

main :: IO ()
main = defaultMain $ testGroup "gigaparsec"
  [ primitiveTests
  ]
