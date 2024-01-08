module Text.Gigaparsec.TokenTests where

import Test.Tasty
import Text.Gigaparsec.Token.NamesTests as Names

tests :: TestTree
tests = testGroup "Token"
  [ Names.tests
  ]
