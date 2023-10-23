{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Errors (module Text.Gigaparsec.Internal.Errors) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (take)
import Data.Set (Set)
import Data.Set qualified as Set (toList, empty, map)

import Text.Gigaparsec.Errors.ErrorBuilder (formatDefault, formatPosDefault, vanillaErrorDefault, specialisedErrorDefault, combineMessagesDefault, disjunct, endOfInputDefault, namedDefault, rawDefault, unexpectedDefault, expectedDefault)

type Span :: *
type Span = Word

type CaretWidth :: *
data CaretWidth = FlexibleCaret { width :: {-# UNPACK #-} !Span }
                | RigidCaret { width :: {-# UNPACK #-} !Span }
                deriving stock Eq

isFlexible :: CaretWidth -> Bool
isFlexible FlexibleCaret{} = True
isFlexible _               = False

-- First pass of the error system will just use a `show` instance within parse,
-- and the tests will use a different parse that exposes the underlying datatype.
-- this will be improved when the error builder is introduced.

type ParseError :: *
data ParseError = VanillaError { presentationOffset :: {-# UNPACK #-} !Word
                               , line :: {-# UNPACK #-} !Word
                               , col :: {-# UNPACK #-} !Word
                               , unexpected :: !(Either Word UnexpectItem)
                               , expecteds :: !(Set ExpectItem)
                               , reasons :: !(Set String)
                               , lexicalError :: !Bool
                               }
                | SpecialisedError { presentationOffset :: {-# UNPACK #-} !Word
                                   , line :: {-# UNPACK #-} !Word
                                   , col :: {-# UNPACK #-} !Word
                                   , msgs :: ![String]
                                   , caretWidth :: {-# UNPACK #-} !Span
                                   }
                deriving stock Eq

type Input :: *
type Input = NonEmpty Char
type UnexpectItem :: *
data UnexpectItem = UnexpectRaw !Input {-# UNPACK #-} !Word
                  | UnexpectNamed !String !CaretWidth
                  | UnexpectEndOfInput
                  deriving stock Eq
type ExpectItem :: *
data ExpectItem = ExpectRaw !String
                | ExpectNamed !String
                | ExpectEndOfInput
                deriving stock (Eq, Ord)

--FIXME: in future, this goes, and we are interacting with the typeclass properly
-- remember that input needs to be processed, and we don't have the residual stream
-- inside show!
instance Show ParseError where
  show :: ParseError -> String
  show err = formatDefault (formatPosDefault (line err) (col err)) Nothing
                           (formatErr err)
    where formatErr VanillaError{..} =
            vanillaErrorDefault (unexpectedDefault (either (const Nothing) (Just . fst . formatUnexpect) unexpected))
                                (expectedDefault (disjunct True (map formatExpectItem (Set.toList expecteds))))
                                (combineMessagesDefault reasons)
                                []
          formatErr SpecialisedError{..} =
            specialisedErrorDefault (combineMessagesDefault msgs)
                                    []

          formatExpectItem (ExpectRaw raw) = rawDefault raw
          formatExpectItem (ExpectNamed name) = namedDefault name
          formatExpectItem ExpectEndOfInput = endOfInputDefault

formatUnexpect :: UnexpectItem -> (String, Span)
formatUnexpect (UnexpectRaw cs demanded) =
  -- TODO: this is MatchParserDemand, but needs to be configurable properly
  (rawDefault (NonEmpty.take (fromIntegral demanded) cs), demanded)
formatUnexpect (UnexpectNamed name caretWidth) = (namedDefault name, width caretWidth)
formatUnexpect UnexpectEndOfInput = (endOfInputDefault, 1)

emptyErr :: Word -> Word -> Word -> Word -> ParseError
emptyErr presentationOffset line col width = VanillaError {
    presentationOffset = presentationOffset,
    line = line,
    col = col,
    unexpected = Left width,
    expecteds = Set.empty,
    reasons = Set.empty,
    lexicalError = False
  }

expectedErr :: String -> Word -> Word -> Word -> Set ExpectItem -> Word -> ParseError
expectedErr input presentationOffset line col expecteds width = VanillaError {
    presentationOffset = presentationOffset,
    line = line,
    col = col,
    unexpected = case nonEmpty input of
      Nothing -> Right UnexpectEndOfInput
      Just cs -> Right (UnexpectRaw cs width),
    expecteds = expecteds,
    reasons = Set.empty,
    lexicalError = False
}

labelErr :: Word -> Set String -> ParseError -> ParseError
labelErr offset expecteds err@VanillaError{}
  | offset == presentationOffset err = err {
      expecteds = Set.map ExpectNamed expecteds
  }
labelErr _ _ err = err
