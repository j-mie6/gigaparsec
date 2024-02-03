{-# LANGUAGE BlockArguments, OverloadedLists #-}
module Text.Gigaparsec.Token.NamesTests where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Gigaparsec
import Text.Gigaparsec.Char (spaces)
import Text.Gigaparsec.Token.Descriptions
import Text.Gigaparsec.Internal.Token.Names (mkNames, lexeme, identifier, Names)

--import Text.Gigaparsec.Internal.Require
import Text.Gigaparsec.Internal.Test
import Control.Monad (forM_)
import Data.Char (isLetter, isAlphaNum)
import Text.Gigaparsec.Internal.TestError
import Text.Gigaparsec.Token.Errors (defaultErrorConfig)

tests :: TestTree
tests = testGroup "Names"
  [ identifierTests
  , userDefinedOperatorTests
  ]

identifierTests :: TestTree
identifierTests = testGroup "identifier should"
  [ testCase "parse valid identifiers" do
      identCases (Just isLetter) (Just isAlphaNum) True
        [ "hello1" --> Just "hello1"
        , "7f" --> Nothing
        , "hi" --> Just "hi"
        , "x7" --> Just "x7"
        ]
      identCases (Just isLetter) Nothing True
        [ "x" --> Just "x"
        , "y" --> Just "y"
        , "hi" --> Just "h"
        , "x7" --> Just "x"
        ]
      identCases (Just (== '🙂')) (Just isAlphaNum) True
        [ "🙂ello1" --> Just "🙂ello1"
        , "df" --> Nothing
        , "🙂i" --> Just "🙂i"
        , "🙂7" --> Just "🙂7"
        , "🙂🙂" --> Just "🙂"
        ]
  , testCase "fail to parse valid keywords" do
      identCases (Just isLetter) (Just isAlphaNum) True
        [ "keyword" --> Nothing
        , "KEYWORD" --> Just "KEYWORD"
        , "keyword1" --> Just "keyword1"
        ]
  , testCase "point at the correct place for the error" do
      case testParseAll (identifier basicNames) "keyword" of
        Failure (TestError pos _) -> pos @?= (1, 1)
        _ -> assertFailure "parser must fail"
  , testCase "report the correct label" do
      case testParseAll (identifier basicNames) "HARD" of
        Failure (TestError _ (VanillaError unex exs rs width)) -> do
          width @?= 4
          unex @?= Just (Named "keyword HARD")
          exs @?= [Named "identifier"]
          rs @?= []
        _ -> assertFailure "parser must fail"
  , testCase "work in the presence of case insensitivity with respect to keywords" do
      identCases (Just isLetter) (Just isAlphaNum) False
        [ "HARD" --> Nothing
        , "hard" --> Nothing
        , "HArd" --> Nothing
        , "harD" --> Nothing
        , "keyword" --> Nothing
        , "Keyword" --> Nothing
        , "keyWORD" --> Nothing
        , "KEYword" --> Nothing
        ]
  ]

userDefinedOperatorTests :: TestTree
userDefinedOperatorTests = testGroup "user defined operator should"
  []

(-->) :: a -> b -> (a, b)
(-->) = (,)

identCases :: CharPredicate -> CharPredicate -> Bool -> [(String, Maybe String)] -> Assertion
identCases start letter sensitive ts =
  do let p = identifier (namesFor start letter sensitive)
     forM_ ts $ \(sym, res) ->
        case res of
          Nothing -> ensureFails p sym
          Just r  -> testParse p sym @?= Success r

namesFor :: CharPredicate -> CharPredicate -> Bool -> Names
namesFor start letter sensitive = lexeme (<* spaces) $
  mkNames (plainName { identifierStart = start, identifierLetter = letter })
          (plainSymbol { caseSensitive = sensitive
                       , hardKeywords = ["keyword", "HARD"]
                       , hardOperators = ["+", "<", "<="]
                       })
          defaultErrorConfig

basicNames :: Names
basicNames = namesFor (Just isLetter) (Just isAlphaNum) True
