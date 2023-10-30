{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module Text.Gigaparsec.ErrorsTests where

import Prelude hiding (fail)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Errors.Combinator
import Text.Gigaparsec.Internal.Test
import Text.Gigaparsec.Internal.TestError

import Data.Set qualified as Set (map)

tests :: TestTree
tests = testGroup "Errors" [ labelTests
                           , hideTests
                           , explainTests
                           , emptyTests
                           , failTests
                           , unexpectedTests
                           , lookAheadTests
                           , notFollowedByTests
                           , oneOfTests
                           , noneOfTests
                           , regressionTests
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

emptyTests :: TestTree
emptyTests = testGroup "empty should"
  [ testCase "produce unknown error messages" do
      testParse @() empty "b" @?= Failure (TestError (1, 1) (VanillaError Nothing [] [] 0))
  , testCase "produce no unknown message under influence of label" do
      testParse @() (empty <?> ["something, at least"]) "b" @?=
        Failure (TestError (1, 1) (VanillaError Nothing [Named "something, at least"] [] 0))
  , testCase "not produce an error message at end of <|> chain" do
      testParse (char 'a' <|> empty) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Raw "a"] [] 1))
  , testCase "produce an expected error under the influence of label in <|> chain" do
      testParse (char 'a' <|> label ["something, at least"] empty) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Raw "a", Named "something, at least"] [] 1))
  , expectFailBecause "no widening for carets in vanilla" $ testCase "have an effect if its caret is wider" do
      testParse (char 'a' <|> emptyWide 3) "bcd" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "bcd")) [Raw "a"] [] 3))
  ]

explainTests :: TestTree
explainTests = ignoreTestBecause "explain not implemented" $ testGroup "explain should"
  [ testCase "provide a message but only on failure" do
      testParse @Int (explain "oops!" empty) "" @?=
        Failure (TestError (1, 1) (VanillaError Nothing [] ["oops!"] 0))
      testParse (explain "requires an a" (char 'a')) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "b")) [Raw "a"] ["requires an a"] 1))
      testParse (explain "an a" (char 'a') <|> explain "a b" (char 'b')) "c" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "c")) [Raw "a", Raw "b"] ["an a", "a b"] 1))
  , testCase "not have any effect when more input has been consumed since it was added" do
      testParse (explain "should be absent" (char 'a') *> char 'b') "a" @?=
        Failure (TestError (1, 2) (VanillaError (Just EndOfInput) [Raw "b"] [] 1))
      testParse (explain "should be absent" (char 'a') <|> (char 'b' *> digit)) "b" @?=
        Failure (TestError (1, 2) (VanillaError (Just EndOfInput) [Named "digit"] [] 1))
  ]

failTests :: TestTree
failTests = testGroup "fail should"
  [ testCase "yield a raw message" do
      testParse @Int (fail ["hi"]) "b" @?=
        Failure (TestError (1, 1) (SpecialisedError ["hi"] 1))
  , expectFailBecause "no cross-error width merging" $ testCase "be flexible when the width is unspecified" do
      testParse (string "abc" <|> fail ["hi"]) "xyz" @?=
        Failure (TestError (1, 1) (SpecialisedError ["hi"] 3))
  , testCase "dominate otherwise" do
      testParse (string "abc" <|> failWide 2 ["hi"]) "xyz" @?=
        Failure (TestError (1, 1) (SpecialisedError ["hi"] 2))
  ]

unexpectedTests :: TestTree
unexpectedTests = testGroup "unexpected should"
  [ testCase "yield changes to unexpected messages" do
      testParse @() (unexpected "bee") "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Named "bee")) [] [] 1))
  , testCase "produce expected message under influence of label, along with original message" do
      testParse (char 'a' <|> label ["something less cute"] (unexpected "bee")) "b" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Named "bee")) [Raw "a", Named "something less cute"] [] 1))
  , expectFailBecause "no widening for carets in vanilla" $ testCase "be flexible when the width is unspecified" do
      testParse (string "abc" <|> unexpected "bee") "xyz" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Named "bee")) [Raw "abc"] [] 3))
  , testCase "dominate otherwise" do
      testParse (string "abc" <|> unexpectedWide 2 "bee") "xyz" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Named "bee")) [Raw "abc"] [] 2))
  ]

lookAheadTests :: TestTree
lookAheadTests = testGroup "lookAhead should"
  [ testCase "produce no hints following it" do
      let p = char 'a' <|> lookAhead (optional digit *> char 'c') <|> char 'b'
      testParse p "d" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "d")) [Raw "a", Raw "b", Raw "c", Named "digit"] [] 1))
      let q = char 'a' <|> lookAhead (optional digit) *> char 'c' <|> char 'b'
      testParse q "d" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "d")) [Raw "a", Raw "b", Raw "c"] [] 1))
      let r = char 'a' <|> lookAhead digit *> char 'c' <|> char 'b'
      testParse r "d" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "d")) [Raw "a", Raw "b", Named "digit"] [] 1))
  ]

notFollowedByTests :: TestTree
notFollowedByTests = testGroup "notFollowedBy should"
  [ testCase "produce no hints" do
      let p = char 'a' <|> notFollowedBy (optional digit) *> char 'c' <|> char 'b'
      testParse p "d" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "d")) [Raw "a", Raw "b"] [] 1))
      let q = char 'a' <|> notFollowedBy digit *> char 'c' <|> char 'b'
      testParse q "d" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "d")) [Raw "a", Raw "b", Raw "c"] [] 1))
  ]

eofTests :: TestTree
eofTests = testGroup "eof should"
  [ testCase "produce expected end of input" do
      testParse eof "a" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "a")) [EndOfInput] [] 1))
  , testCase "change message under the influence of label" do
      testParse (label ["something more"] eof) "a" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "a")) [Named "something more"] [] 1))
  ]

--TODO: amend/entrench/dislodge tests
--TODO: filter tests

oneOfTests :: TestTree
oneOfTests = testGroup "oneOf should"
  [ testCase "incorporate range notation into the error" do
      testParse (oneOf ['0' .. '9']) "a" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "a")) [Named "one of \"0\" to \"9\""] [] 1))
  , testCase "incorporate sets of characters into error" do
      testParse (oneOf ['0', '2' .. '9']) "a" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "a")) (Set.map (Named . show . (: [])) ['0', '2' .. '9']) [] 1))
  ]

noneOfTests :: TestTree
noneOfTests = testGroup "noneOf should"
  [ testCase "incorporate range notation into the error" do
      testParse (noneOf ['0' .. '9']) "8" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "8")) [Named "anything outside of \"0\" to \"9\""] [] 1))
  , expectFailBecause "no label applied yet" $ testCase "incorporate sets of characters into error" do
      testParse (noneOf ['0', '2' .. '9']) "8" @?=
        Failure (TestError (1, 1) (VanillaError (Just (Raw "8")) [Named "anything except \"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", or \"9\""] [] 1))
  ]

--TODO: patterns tests

regressionTests :: TestTree
regressionTests = testGroup "thou shalt not regress"
  [ testGroup "hints should"
      [ testCase "incorporate only with errors at the same offset depth" do
          let p = atomic (char 'a' *> digit)
          let parser = optional (char 'b' <?> ["b"]) *> label ["foo"] p
          case testParse parser "aa" of
            Failure (TestError _ (VanillaError _ expecteds _ 1)) -> do
              expecteds @?= [Named "digit"]
            err -> assertFailure $ "error message " ++ show err ++ " did not match"
          --TODO: when amend is done
          {-let q = amend (char 'a' *> digit)
          let qarser = optional (char 'b' <?> ["b"]) *> label ["foo"] q
          case testParse qarser "aa" of
            Failure (TestError _ (VanillaError _ expecteds _ 1)) -> do
              expecteds @?= [Named "foo", Named "b"]
            err -> assertFailure $ "error message " ++ show err ++ " did not match"-}
      ]
  ]
