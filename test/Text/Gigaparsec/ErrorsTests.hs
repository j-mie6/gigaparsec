{-# LANGUAGE OverloadedLists #-}
module Text.Gigaparsec.ErrorsTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Errors.Combinator
import Text.Gigaparsec.Internal.Test
import Text.Gigaparsec.Internal.TestError

tests :: TestTree
tests = testGroup "Errors" [ labelTests
                           , hideTests
                           ]

labelTests :: TestTree
labelTests = testGroup "label should"
  [ testCase "affect base error messages" do
      testParse (char 'a' <?> ["ay!"]) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Named "ay!"] [] 1))
      testParse (char 'a' <?> ["ay!", "see!"]) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Named "ay!", Named "see!"] [] 1))
  , testCase "work across a recursion boundary" do
      let r = string "correct error message" <:> r
      let p = r <?> ["nothing but this :)"]
      testParse p "" @?=
        Failure (TestError (1, 1) (VanillaError (Just EndOfInput) [Named "nothing but this :)"] [] 1))
      testParse p "correct error message" @?=
        Failure (TestError (1, 22) (VanillaError (Just EndOfInput) [Raw "correct error message"] [] 1))
  , testCase "replace everything under the label" do
      let s = label ["hi"] (optional (char 'a') *> optional (char 'b')) *> char 'c'
      testParse s "e" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "e")) [Named "hi", Raw "c"] [] 1))
      let t = label ["hi"] (optional (char 'a') *> label ["bee"] (optional (char 'b'))) *> char 'c'
      testParse t "e" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "e")) [Named "hi", Raw "c"] [] 1))
      testParse t "ae" @?=
        Failure (TestError (1, 2) (VanillaError (Just (Raw "e")) [Named "bee", Raw "c"] [] 1))
      let v = label ["hi"] (hide (optional (char 'a')) *> label ["bee"] (optional (char 'b'))) *> char 'c'
      testParse v "e" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "e")) [Named "hi", Raw "c"] [] 1))
      testParse v "ae" @?=
        Failure (TestError (1, 2) (VanillaError (Just (Raw "e")) [Named "bee", Raw "c"] [] 1))
  , testCase "should not replace hints if input is consumed" do
      testParse ((many digit <?> ["number"]) <* eof) "1e" @?=
        Failure (TestError (1, 2) (VanillaError (Just (Raw "e")) [Named "digit", EndOfInput] [] 1))
  ]

hideTests :: TestTree
hideTests = testGroup "hide should"
  [ testCase "not produce any visible output" do
      testParse (hide (char 'a')) "" @?=
        Failure (TestError (1, 1) (VanillaError Nothing [] [] 0))
      testParse (hide (string "a")) "" @?=
        Failure (TestError (1, 1) (VanillaError Nothing [] [] 0))
      testParse (hide digit) "" @?=
        Failure (TestError (1, 1) (VanillaError Nothing [] [] 0))
  , testCase "suppress hints even if input is consumed" do
      testParse (hide (many digit) <* eof) "1e" @?=
        Failure (TestError (1, 2) (VanillaError (Just (Raw "e")) [EndOfInput] [] 1))
  , testCase "not allow hints to be unsuppressed by another label" do
      testParse (label ["hey"] (hide (many digit)) <* eof) "1e" @?=
        Failure (TestError (1, 2) (VanillaError (Just (Raw "e")) [EndOfInput] [] 1))
  ]
