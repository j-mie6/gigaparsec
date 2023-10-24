{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards, BangPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Errors (module Text.Gigaparsec.Internal.Errors) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (take)
import Data.Set (Set)
import Data.Set qualified as Set (toList, empty, map, union)

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
                                   --, caretWidth :: {-# UNPACK #-} !Span --FIXME: need defunc before this goes away
                                   , caretWidth :: !CaretWidth
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

specialisedErr :: Word -> Word -> Word -> [String] -> CaretWidth -> ParseError
specialisedErr presentationOffset line col msgs caretWidth = SpecialisedError {..}

labelErr :: Word -> Set String -> ParseError -> ParseError
labelErr offset expecteds err@VanillaError{}
  | offset == presentationOffset err = err {
      expecteds = Set.map ExpectNamed expecteds
  }
labelErr _ _ err = err

mergeErr :: ParseError -> ParseError -> ParseError
mergeErr !err1 !err2
  -- TODO: underlyingOffset comparison
  | presentationOffset err1 > presentationOffset err2 = err1
  | presentationOffset err1 < presentationOffset err2 = err2
-- offsets are all equal, kinds must match
mergeErr err1@SpecialisedError{caretWidth} _err2@VanillaError{}
  | isFlexible caretWidth = err1 -- TODO: flexible caret merging from err2
  | otherwise             = err1
mergeErr _err1@VanillaError{} err2@SpecialisedError{caretWidth}
  | isFlexible caretWidth = err2 -- TODO: flexible caret merging from err1
  | otherwise             = err2
mergeErr err1@VanillaError{} err2@VanillaError{} =
  err1 { unexpected = mergeUnexpect (unexpected err1) (unexpected err2)
       , expecteds = Set.union (expecteds err1) (expecteds err2)
       , reasons = Set.union (reasons err1) (reasons err2)
       , lexicalError = lexicalError err1 || lexicalError err2
       }
mergeErr err1@SpecialisedError{} err2@SpecialisedError{} =
  err1 { msgs = msgs err1 ++ msgs err2
       , caretWidth = mergeCaret (caretWidth err1) (caretWidth err2)
       }

mergeCaret :: CaretWidth -> CaretWidth -> CaretWidth
mergeCaret caret@RigidCaret{} FlexibleCaret{} = caret
mergeCaret FlexibleCaret{} caret@RigidCaret{} = caret
mergeCaret caret1 caret2 = caret1 { width = max (width caret1) (width caret2) }

mergeUnexpect :: Either Word UnexpectItem -> Either Word UnexpectItem -> Either Word UnexpectItem
mergeUnexpect (Left w1) (Left w2) = Left (max w1 w2)
-- TODO: widening can occur with flexible or raw tokens
mergeUnexpect Left{} w@Right{} = w
mergeUnexpect w@Right{} Left{} = w
-- finally, two others will merge independently
mergeUnexpect (Right item1) (Right item2) = Right (mergeItem item1 item2)
  where mergeItem UnexpectEndOfInput _ = UnexpectEndOfInput
        mergeItem _ UnexpectEndOfInput = UnexpectEndOfInput
        mergeItem it1@(UnexpectNamed _ cw1) it2@(UnexpectNamed _ cw2)
          | isFlexible cw1, not (isFlexible cw2) = it2
          | not (isFlexible cw1), isFlexible cw2 = it1
          | width cw1 < width cw2                = it2
          | otherwise                            = it1
        mergeItem item@UnexpectNamed{} _ = item
        mergeItem _ item@UnexpectNamed{} = item
        mergeItem (UnexpectRaw cs w1) (UnexpectRaw _ w2) = UnexpectRaw cs (max w1 w2)
