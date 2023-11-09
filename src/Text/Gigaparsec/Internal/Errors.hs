{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards, BangPatterns, NamedFieldPuns, CPP #-}
#include "portable-unlifted.h"
{-# OPTIONS_GHC -Wno-partial-fields -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Internal.Errors (module Text.Gigaparsec.Internal.Errors) where

import Prelude hiding (lines)

import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty, (<|))
import Data.Set (Set)
import Data.Set qualified as Set (empty, map, union, null, foldr, insert)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder, tokenSpan)
import Text.Gigaparsec.Errors.ErrorBuilder qualified as Builder (ErrorBuilder(..))
import Text.Gigaparsec.Errors.ErrorBuilder qualified as Token (Token(..))

CPP_import_PortableUnlifted

type Span :: *
type Span = Word

type CaretWidth :: UnliftedDatatype
data CaretWidth = FlexibleCaret { width :: {-# UNPACK #-} !Span }
                | RigidCaret { width :: {-# UNPACK #-} !Span }

isFlexible :: CaretWidth -> Bool
isFlexible FlexibleCaret{} = True
isFlexible _               = False

type ParseError :: UnliftedDatatype
data ParseError = VanillaError { presentationOffset :: {-# UNPACK #-} !Word
                               , line :: {-# UNPACK #-} !Word
                               , col :: {-# UNPACK #-} !Word
                               , unexpected :: !(Either Word UnexpectItem) -- TODO: unlift this!
                               -- sadly, this prevents unlifting of ExpectItem
                               -- perhaps we should make an unlifted+levity polymorphic Set?
                               , expecteds :: !(Set ExpectItem)
                               , reasons :: !(Set String)
                               , lexicalError :: !Bool -- TODO: strict bools
                               -- TODO: remove:
                               , underlyingOffset :: {-# UNPACK #-} !Word
                               , entrenchment :: {-# UNPACK #-} !Word
                               }
                | SpecialisedError { presentationOffset :: {-# UNPACK #-} !Word
                                   , line :: {-# UNPACK #-} !Word
                                   , col :: {-# UNPACK #-} !Word
                                   , msgs :: ![String]
                                   --, caretWidth :: {-# UNPACK #-} !Span --FIXME: need defunc before this goes away
                                   , caretWidth :: CaretWidth
                                   -- TODO: remove:
                                   , underlyingOffset :: {-# UNPACK #-} !Word
                                   , entrenchment :: {-# UNPACK #-} !Word
                                   }

type Input :: *
type Input = NonEmpty Char
type UnexpectItem :: *
data UnexpectItem = UnexpectRaw !Input {-# UNPACK #-} !Word
                  | UnexpectNamed !String CaretWidth
                  | UnexpectEndOfInput
type ExpectItem :: *
data ExpectItem = ExpectRaw !String
                | ExpectNamed !String
                | ExpectEndOfInput
                deriving stock (Eq, Ord, Show)

entrenched :: ParseError -> Bool
entrenched err = entrenchment err /= 0

emptyErr :: Word -> Word -> Word -> Word -> ParseError
emptyErr !presentationOffset !line !col !width = VanillaError {
    presentationOffset = presentationOffset,
    line = line,
    col = col,
    unexpected = Left width,
    expecteds = Set.empty,
    reasons = Set.empty,
    lexicalError = False,
    underlyingOffset = presentationOffset,
    entrenchment = 0
  }

expectedErr :: String -> Word -> Word -> Word -> Set ExpectItem -> Word -> ParseError
expectedErr !input !presentationOffset !line !col !expecteds !width = VanillaError {
    presentationOffset = presentationOffset,
    line = line,
    col = col,
    unexpected = case nonEmpty input of
      Nothing -> Right UnexpectEndOfInput
      Just cs -> Right (UnexpectRaw cs width),
    expecteds = expecteds,
    reasons = Set.empty,
    lexicalError = False,
    underlyingOffset = presentationOffset,
    entrenchment = 0
}

specialisedErr :: Word -> Word -> Word -> [String] -> CaretWidth -> ParseError
specialisedErr !presentationOffset !line !col !msgs caretWidth = SpecialisedError {..}
  where !underlyingOffset = presentationOffset
        !entrenchment = 0 :: Word

unexpectedErr :: Word -> Word -> Word -> Set ExpectItem -> String -> CaretWidth -> ParseError
unexpectedErr !presentationOffset !line !col !expecteds !name caretWidth = VanillaError {
    presentationOffset = presentationOffset,
    line = line,
    col = col,
    expecteds = expecteds,
    unexpected = Right (UnexpectNamed name caretWidth),
    reasons = Set.empty,
    lexicalError = False,
    underlyingOffset = presentationOffset,
    entrenchment = 0
  }

labelErr :: Word -> Set String -> ParseError -> ParseError
labelErr !offset expecteds err@VanillaError{}
  | offset == presentationOffset err = err { expecteds = Set.map ExpectNamed expecteds }
labelErr _ _ err = err

explainErr :: Word -> String -> ParseError -> ParseError
explainErr !offset reason err@VanillaError{}
  | offset == presentationOffset err = err { reasons = Set.insert reason (reasons err) }
explainErr _ _ err = err

amendErr :: Word -> Word -> Word -> ParseError -> ParseError
amendErr !offset !line !col err
  | not (entrenched err) = err {
      presentationOffset = offset,
      underlyingOffset = offset,
      line = line,
      col = col
    }
amendErr _ _ _ err = err

partialAmendErr :: Word -> Word -> Word -> ParseError -> ParseError
partialAmendErr !offset !line !col err
  | not (entrenched err) =  err {
      presentationOffset = offset,
      line = line,
      col = col
    }
partialAmendErr _ _ _ err = err

entrenchErr :: ParseError -> ParseError
entrenchErr err = err { entrenchment = entrenchment err + 1 }

dislodgeErr :: Word -> ParseError -> ParseError
dislodgeErr by err
  | entrenchment err == 0  = err
  -- this case is important to avoid underflow on the unsigned Word
  | by >= entrenchment err = err { entrenchment = 0 }
  | otherwise              = err { entrenchment = entrenchment err - by }

setLexical :: ParseError -> ParseError
setLexical err@VanillaError{} = err { lexicalError = True }
setLexical err = err

useHints :: Set ExpectItem -> ParseError -> ParseError
useHints !hints err@VanillaError{expecteds} = err { expecteds = Set.union hints expecteds }
useHints _ err = err

mergeErr :: ParseError -> ParseError -> ParseError
mergeErr err1 err2
  | underlyingOffset err1 > underlyingOffset err2 = err1
  | underlyingOffset err1 < underlyingOffset err2 = err2
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

isExpectedEmpty :: ParseError -> Bool
isExpectedEmpty VanillaError{expecteds} = Set.null expecteds
isExpectedEmpty _                       = True

{-# INLINABLE fromParseError #-}
fromParseError :: forall err. ErrorBuilder err => Maybe FilePath -> String -> ParseError -> err
fromParseError srcFile input err =
  Builder.format (Builder.pos @err (line err) (col err)) (Builder.source @err srcFile)
                 (formatErr err)
  where formatErr :: ParseError -> Builder.ErrorInfoLines err
        formatErr VanillaError{..} =
          Builder.vanillaError @err
            (Builder.unexpected @err (either (const Nothing) (Just . fst) unexpectedTok))
            (Builder.expected @err (Builder.combineExpectedItems @err (Set.map expectItem expecteds)))
            (Builder.combineMessages @err (Set.foldr (\r -> (Builder.reason @err r :)) [] reasons))
            (Builder.lineInfo @err curLine linesBefore linesAfter caret (trimToLine caretSize))
          where unexpectedTok = unexpectItem lexicalError <$> unexpected
                caretSize = either id snd unexpectedTok

        formatErr SpecialisedError{..} =
          Builder.specialisedError @err
            (Builder.combineMessages @err (map (Builder.message @err) msgs))
            (Builder.lineInfo @err curLine linesBefore linesAfter caret (trimToLine (width caretWidth)))

        expectItem :: ExpectItem -> Builder.Item err
        expectItem (ExpectRaw t) = Builder.raw @err t
        expectItem (ExpectNamed n) = Builder.named @err n
        expectItem ExpectEndOfInput = Builder.endOfInput @err

        unexpectItem :: Bool -> UnexpectItem -> (Builder.Item err, Span)
        unexpectItem lexical (UnexpectRaw cs demanded) =
          case Builder.unexpectedToken @err cs demanded lexical of
            t@(Token.Raw tok) -> (Builder.raw @err tok, tokenSpan t)
            Token.Named name w -> (Builder.named @err name, w)
        unexpectItem _ (UnexpectNamed name caretWidth) = (Builder.named @err name, width caretWidth)
        unexpectItem _ UnexpectEndOfInput = (Builder.endOfInput @err, 1)

        -- it is definitely the case that there are at least `line` lines
        (allLinesBefore, curLine, allLinesAfter) = breakLines (line err - 1) (lines input)
        linesBefore = drop (length allLinesBefore - Builder.numLinesBefore @err) allLinesBefore
        linesAfter = take (Builder.numLinesAfter @err) allLinesAfter

        caret = col err - 1
        trimToLine width = min width (fromIntegral (length curLine) - caret + 1)

        lines :: String -> NonEmpty String
        lines [] = "" :| []
        lines ('\n':cs) = "" <| lines cs
        lines (c:cs) = let l :| ls = lines cs in (c:l) :| ls

        breakLines :: Word -> NonEmpty String -> ([String], String, [String])
        breakLines 0 (l :| ls) = ([], l, ls)
        breakLines n (l :| ls) = case nonEmpty ls of
          Nothing -> error "the focus line is guaranteed to exist"
          Just ls' -> let (before, focus, after) = breakLines (n - 1) ls'
                      in (l : before, focus, after)
