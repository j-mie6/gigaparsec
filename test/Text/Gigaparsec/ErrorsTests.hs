{-# LANGUAGE OverloadedLists #-}
module Text.Gigaparsec.ErrorsTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Errors.Combinator
import Text.Gigaparsec.Internal.Test
import Text.Gigaparsec.Internal.TestError

tests :: TestTree
tests = testGroup "Errors" []

labelTests :: TestTree
labelTests = testGroup "label should"
  [ testCase "affect base error messages" do
      testParse (char 'a' <?> ["ay!"]) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Named "ay!"] [] 1))
      testParse (char 'a' <?> ["ay!", "see!"]) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Named "ay!", Named "see!"] [] 1))
  ]
