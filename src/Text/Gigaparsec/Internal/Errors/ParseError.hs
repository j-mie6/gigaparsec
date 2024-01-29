{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-missing-import-lists #-}
module Text.Gigaparsec.Internal.Errors.ParseError (
    module Text.Gigaparsec.Internal.Errors.ParseError
  ) where

import Prelude hiding (lines)

import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty, (<|))
import Data.Set qualified as Set (map, foldr)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder, Token)
import Text.Gigaparsec.Errors.ErrorBuilder qualified as Builder (ErrorBuilder(..))
import Text.Gigaparsec.Errors.ErrorBuilder qualified as Token (Token(..))

import Text.Gigaparsec.Internal.Errors.CaretControl
import Text.Gigaparsec.Internal.Errors.ErrorItem

import Data.Set (Set)

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
            (Builder.lineInfo @err curLine linesBefore linesAfter caret (trimToLine caretWidth))

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

        tokenSpan :: Token -> Word
        tokenSpan (Token.Raw cs) = fromIntegral (length cs)
        tokenSpan (Token.Named _ w) = w
