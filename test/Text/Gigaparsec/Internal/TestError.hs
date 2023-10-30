{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Text.Gigaparsec.Internal.TestError (
    TestError(..), TestErrorLines(..), TestErrorItem(..), parseErrorToTestError
  ) where

import Text.Gigaparsec.Internal.Errors qualified as Errors
import Text.Gigaparsec.Errors.ErrorBuilder
import Text.Gigaparsec.Errors.Token qualified as Token

import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set (fromList, map)
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

parseErrorToTestError :: Errors.ParseError -> TestError
parseErrorToTestError Errors.VanillaError{..} =
  TestError (line, col) $ VanillaError (either (const Nothing) (Just . unexpectItem) unexpected)
                                       (Set.map expectItem expecteds)
                                       reasons
                                       (vanillaWidth unexpected)
parseErrorToTestError Errors.SpecialisedError{..} =
  TestError (line, col) $ SpecialisedError (Set.fromList msgs) (Errors.width caretWidth)

unexpectItem :: Errors.UnexpectItem -> TestErrorItem
unexpectItem Errors.UnexpectEndOfInput = EndOfInput
unexpectItem (Errors.UnexpectNamed name _) = Named name
unexpectItem (Errors.UnexpectRaw cs width) = Raw (NonEmpty.take (fromIntegral width) cs)

expectItem :: Errors.ExpectItem -> TestErrorItem
expectItem Errors.ExpectEndOfInput = EndOfInput
expectItem (Errors.ExpectNamed name) = Named name
expectItem (Errors.ExpectRaw cs) = Raw cs

vanillaWidth :: Either Word Errors.UnexpectItem -> Word
vanillaWidth (Left w) = w
vanillaWidth (Right Errors.UnexpectEndOfInput) = 1
vanillaWidth (Right (Errors.UnexpectNamed _ cw)) = Errors.width cw
vanillaWidth (Right (Errors.UnexpectRaw _ w)) = w
