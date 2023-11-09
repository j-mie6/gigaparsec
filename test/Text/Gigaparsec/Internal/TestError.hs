{-# LANGUAGE TypeFamilies #-}
module Text.Gigaparsec.Internal.TestError (
    TestError(..), TestErrorLines(..), TestErrorItem(..)
  ) where

import Text.Gigaparsec.Errors.ErrorBuilder hiding (Token(..))
import Text.Gigaparsec.Errors.ErrorBuilder qualified as Token

import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set (fromList)
import Data.List.NonEmpty qualified as NonEmpty (take)

data TestError = TestError !(Word, Word) !TestErrorLines deriving stock (Eq, Show, Ord)
data TestErrorLines = VanillaError !(Maybe TestErrorItem) !(Set TestErrorItem) !(Set String) !Word
                    | SpecialisedError !(Set String) !Word deriving stock (Eq, Show, Ord)
data TestErrorItem = Raw !String | Named !String | EndOfInput deriving stock (Eq, Show, Ord)

instance ErrorBuilder TestError where
  format p _ = TestError p

  type Position TestError = (Word, Word)
  pos = (,)

  type Source TestError = ()
  source = const ()

  type ErrorInfoLines TestError = TestErrorLines
  vanillaError = VanillaError
  specialisedError = SpecialisedError

  type ExpectedItems TestError = Set TestErrorItem
  combineExpectedItems = id

  type Messages TestError = Set String
  combineMessages = Set.fromList

  type UnexpectedLine TestError = Maybe TestErrorItem
  unexpected = id
  type ExpectedLine TestError = Set TestErrorItem
  expected = id

  type Message TestError = String
  reason = id
  message = id

  type LineInfo TestError = Word
  lineInfo _ _ _ _ width = width

  numLinesBefore = 2
  numLinesAfter = 2

  type Item TestError = TestErrorItem
  raw = Raw
  named = Named
  endOfInput = EndOfInput

  unexpectedToken :: NonEmpty Char -> Word -> Bool -> Token.Token
  unexpectedToken cs demanded _ = Token.Raw (NonEmpty.take (fromIntegral demanded) cs)
