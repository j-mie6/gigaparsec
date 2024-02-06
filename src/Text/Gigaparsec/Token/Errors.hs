{-# LANGUAGE Safe #-}
{-# LANGUAGE NoMonomorphismRestriction, BlockArguments, OverloadedLists, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Text.Gigaparsec.Token.Errors (
    ErrorConfig,
    labelNumericBreakChar, labelIntegerUnsignedDecimal,
    labelIntegerUnsignedHexadecimal, labelIntegerUnsignedOctal,
    labelIntegerUnsignedBinary, labelIntegerUnsignedNumber,
    labelIntegerSignedDecimal,
    labelIntegerSignedHexadecimal, labelIntegerSignedOctal,
    labelIntegerSignedBinary, labelIntegerSignedNumber,
    labelIntegerDecimalEnd,
    labelIntegerHexadecimalEnd, labelIntegerOctalEnd,
    labelIntegerBinaryEnd, labelIntegerNumberEnd,
    filterIntegerOutOfBounds,
    labelNameIdentifier, labelNameOperator,
    unexpectedNameIllegalIdentifier, unexpectedNameIllegalOperator,
    filterNameIllFormedIdentifier, filterNameIllFormedOperator,
    labelCharAscii, labelCharLatin1, labelCharUnicode,
    labelCharAsciiEnd, labelCharLatin1End, labelCharUnicodeEnd,
    labelStringAscii, labelStringLatin1, labelStringUnicode,
    labelStringAsciiEnd, labelStringLatin1End, labelStringUnicodeEnd,
    labelStringCharacter, labelGraphicCharacter, labelEscapeSequence,
    labelEscapeNumeric, labelEscapeNumericEnd, labelEscapeEnd,
    labelStringEscapeEmpty, labelStringEscapeGap, labelStringEscapeGapEnd,
    filterCharNonAscii, filterCharNonLatin1, filterStringNonAscii, filterStringNonLatin1,
    filterEscapeCharRequiresExactDigits, filterEscapeCharNumericSequenceIllegal,
    verifiedCharBadCharsUsedInLiteral, verifiedStringBadCharsUsedInLiteral,
    labelSymbol, labelSymbolEndOfKeyword, labelSymbolEndOfOperator,
    labelSpaceEndOfLineComment, labelSpaceEndOfMultiComment,
    defaultErrorConfig,
    LabelWithExplainConfig, LabelWithExplainConfigurable(..),
    LabelConfig, LabelConfigurable(..),
    ExplainConfig, ExplainConfigurable(..),
    NotConfigurable(..),
    FilterConfig,
    VanillaFilterConfig, VanillaFilterConfigurable(..),
    SpecializedFilterConfig, SpecializedFilterConfigurable(..),
    BasicFilterConfigurable(..),
    VerifiedBadChars, badCharsFail, badCharsReason,
    Unverified(..),
    Bits(B8, B16, B32, B64)
  ) where

import Data.Set (Set)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Kind (Constraint)
import Text.Gigaparsec.Internal.Token.BitBounds (Bits(B8, B16, B32, B64))
import Numeric (showIntAtBase)
import Data.Char (intToDigit, ord)
import Text.Gigaparsec.Errors.DefaultErrorBuilder (from, disjunct, toString)
import Text.Gigaparsec.Internal.Token.Errors (
    LabelWithExplainConfig(LELabelAndReason, LELabel, LEHidden, LEReason, LENotConfigured),
    LabelConfig(LLabel, LHidden, LNotConfigured), ExplainConfig(EReason, ENotConfigured),
    FilterConfig(VSBecause, VSUnexpected, VSUnexpectedBecause, VSBasicFilter, VSSpecializedFilter),
    SpecializedFilterConfig(SSpecializedFilter, SBasicFilter),
    VanillaFilterConfig(VBecause, VUnexpected, VUnexpectedBecause, VBasicFilter),
    VerifiedBadChars(BadCharsUnverified, BadCharsFail, BadCharsReason)
  )

type ErrorConfig :: *
data ErrorConfig =
  ErrorConfig { labelNumericBreakChar :: !LabelWithExplainConfig
              , labelIntegerUnsignedDecimal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerUnsignedHexadecimal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerUnsignedOctal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerUnsignedBinary :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerUnsignedNumber :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerSignedDecimal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerSignedHexadecimal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerSignedOctal :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerSignedBinary :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerSignedNumber :: Maybe Bits -> LabelWithExplainConfig
              , labelIntegerDecimalEnd :: LabelConfig
              , labelIntegerHexadecimalEnd :: LabelConfig
              , labelIntegerOctalEnd :: LabelConfig
              , labelIntegerBinaryEnd :: LabelConfig
              , labelIntegerNumberEnd :: LabelConfig
              , filterIntegerOutOfBounds :: Integer -> Integer -> Int -> FilterConfig Integer
              , labelNameIdentifier :: String
              , labelNameOperator :: String
              , unexpectedNameIllegalIdentifier :: String -> String
              , unexpectedNameIllegalOperator :: String -> String
              , filterNameIllFormedIdentifier :: FilterConfig String
              , filterNameIllFormedOperator :: FilterConfig String
              , labelCharAscii :: LabelWithExplainConfig
              , labelCharLatin1 :: LabelWithExplainConfig
              , labelCharUnicode :: LabelWithExplainConfig
              , labelCharAsciiEnd :: LabelConfig
              , labelCharLatin1End :: LabelConfig
              , labelCharUnicodeEnd :: LabelConfig
              , labelStringAscii :: Bool -> Bool -> LabelWithExplainConfig
              , labelStringLatin1 :: Bool -> Bool -> LabelWithExplainConfig
              , labelStringUnicode :: Bool -> Bool -> LabelWithExplainConfig
              , labelStringAsciiEnd :: Bool -> Bool -> LabelConfig
              , labelStringLatin1End :: Bool -> Bool -> LabelConfig
              , labelStringUnicodeEnd :: Bool -> Bool -> LabelConfig
              , labelStringCharacter :: LabelConfig
              , labelGraphicCharacter :: LabelWithExplainConfig
              , labelEscapeSequence :: LabelWithExplainConfig
              , labelEscapeNumeric :: Int -> LabelWithExplainConfig
              , labelEscapeNumericEnd :: Char -> Int -> LabelWithExplainConfig
              , labelEscapeEnd :: LabelWithExplainConfig
              , labelStringEscapeEmpty :: LabelConfig
              , labelStringEscapeGap :: LabelConfig
              , labelStringEscapeGapEnd :: LabelConfig
              , filterCharNonAscii :: VanillaFilterConfig Char
              , filterCharNonLatin1 :: VanillaFilterConfig Char
              , filterStringNonAscii :: SpecializedFilterConfig String
              , filterStringNonLatin1 :: SpecializedFilterConfig String
              , filterEscapeCharRequiresExactDigits :: Int -> NonEmpty Word -> SpecializedFilterConfig Word
              , filterEscapeCharNumericSequenceIllegal :: Char -> Int -> SpecializedFilterConfig Integer
              , verifiedCharBadCharsUsedInLiteral :: VerifiedBadChars
              , verifiedStringBadCharsUsedInLiteral :: VerifiedBadChars
              , labelSymbol :: Map String LabelWithExplainConfig
              -- don't bother with these until parsley standardises
              --, defaultSymbolKeyword :: Labeller
              --, defaultSymbolOperator :: Labeller
              --, defaultSymbolPunctuaton :: Labeller
              , labelSymbolEndOfKeyword :: String -> String
              , labelSymbolEndOfOperator :: String -> String
              , labelSpaceEndOfLineComment :: LabelWithExplainConfig
              , labelSpaceEndOfMultiComment :: LabelWithExplainConfig
              }

defaultErrorConfig :: ErrorConfig
defaultErrorConfig = ErrorConfig {..}
  where labelNumericBreakChar = notConfigured
        labelIntegerUnsignedDecimal = const notConfigured
        labelIntegerUnsignedHexadecimal = const notConfigured
        labelIntegerUnsignedOctal = const notConfigured
        labelIntegerUnsignedBinary = const notConfigured
        labelIntegerUnsignedNumber = const notConfigured
        labelIntegerSignedDecimal = const notConfigured
        labelIntegerSignedHexadecimal = const notConfigured
        labelIntegerSignedOctal = const notConfigured
        labelIntegerSignedBinary = const notConfigured
        labelIntegerSignedNumber = const notConfigured
        labelIntegerDecimalEnd = notConfigured
        labelIntegerHexadecimalEnd = notConfigured
        labelIntegerOctalEnd = notConfigured
        labelIntegerBinaryEnd = notConfigured
        labelIntegerNumberEnd = notConfigured
        filterIntegerOutOfBounds small big nativeRadix = specializedFilter
          (outOfBounds small big nativeRadix)
        labelNameIdentifier = "identifier"
        labelNameOperator = "operator"
        unexpectedNameIllegalIdentifier = ("keyword " ++)
        unexpectedNameIllegalOperator = ("reserved operator " ++)
        filterNameIllFormedIdentifier = unexpected ("identifier " ++)
        filterNameIllFormedOperator = unexpected ("operator " ++)
        labelCharAscii = notConfigured
        labelCharLatin1 = notConfigured
        labelCharUnicode = notConfigured
        labelCharAsciiEnd = notConfigured
        labelCharLatin1End = notConfigured
        labelCharUnicodeEnd = notConfigured
        labelStringAscii _ _ = notConfigured
        labelStringLatin1 _ _ = notConfigured
        labelStringUnicode _ _ = notConfigured
        labelStringAsciiEnd _ _ = notConfigured
        labelStringLatin1End _ _ = notConfigured
        labelStringUnicodeEnd _ _ = notConfigured
        labelStringCharacter = label ["string character"]
        labelGraphicCharacter = label ["graphic character"]
        labelEscapeSequence = label ["escape sequence"]
        labelEscapeNumeric _ = notConfigured
        labelEscapeNumericEnd _ _ = notConfigured
        labelEscapeEnd = labelAndReason ["end of escape sequence"] "invalid escape sequence"
        labelStringEscapeEmpty = notConfigured
        labelStringEscapeGap = label ["string gap"]
        labelStringEscapeGapEnd = label ["end of string gap"]
        filterCharNonAscii = because (const "non-ascii character")
        filterCharNonLatin1 = because (const "non-latin1 character")
        filterStringNonAscii =
          specializedFilter (const ["non-ascii characters in string literal, this is not allowed"])
        filterStringNonLatin1 =
          specializedFilter (const ["non-latin1 characters in string literal, this is not allowed"])
        filterEscapeCharRequiresExactDigits _ needed = specializedFilter \got ->
          let ~(Just formatted) = disjunct True (map show (NonEmpty.toList needed))
          in [toString ("numeric escape requires " <> formatted <> "digits, but only got" <> from got)]
        filterEscapeCharNumericSequenceIllegal maxEscape radix =
          let messages :: Integer -> NonEmpty String
              messages c
                | c > toInteger (ord maxEscape) = singleton $
                    showIntAtBase (toInteger radix) intToDigit c
                      (" is greater than the maximum character value of "
                      ++ showIntAtBase (toInteger radix) intToDigit (toInteger (ord maxEscape)) "")
                | otherwise = singleton $ "illegal unicode character: "
                                        ++ showIntAtBase (toInteger radix) intToDigit c ""
          in specializedFilter messages
        verifiedCharBadCharsUsedInLiteral = unverified
        verifiedStringBadCharsUsedInLiteral = unverified
        labelSymbol = Map.empty
        -- defaultSymbolKeyword = Label
        -- defaultSymbolOperator = Label
        -- defaultSymbolOperator = NotConfigured
        labelSymbolEndOfKeyword = ("end of " ++)
        labelSymbolEndOfOperator = ("end of " ++)
        labelSpaceEndOfLineComment = label ["end of comment"]
        labelSpaceEndOfMultiComment = label ["end of comment"]

outOfBounds :: Integer -> Integer -> Int -> Integer -> NonEmpty String
outOfBounds small big radix _n = singleton $
    "literal is not within the range " ++ resign small (" to " ++ resign big "")
  where resign n
          | n < 0 = ('-' :) . showIntAtBase (toInteger radix) intToDigit (abs n)
          | otherwise = showIntAtBase (toInteger radix) intToDigit n

type LabelConfigurable :: * -> Constraint
class LabelConfigurable config where
  label :: Set String -> config
  hidden :: config

instance LabelConfigurable LabelConfig where
  label = LLabel
  hidden = LHidden
instance LabelConfigurable LabelWithExplainConfig where
  label = LELabel
  hidden = LEHidden

type ExplainConfigurable :: * -> Constraint
class ExplainConfigurable config where
  reason :: String -> config

instance ExplainConfigurable ExplainConfig where reason = EReason
instance ExplainConfigurable LabelWithExplainConfig where reason = LEReason

type LabelWithExplainConfigurable :: * -> Constraint
class LabelWithExplainConfigurable config where
  labelAndReason :: Set String -> String -> config

instance LabelWithExplainConfigurable LabelWithExplainConfig where labelAndReason = LELabelAndReason

type NotConfigurable :: * -> Constraint
class NotConfigurable config where
  notConfigured :: config

instance NotConfigurable LabelWithExplainConfig where notConfigured = LENotConfigured
instance NotConfigurable LabelConfig where notConfigured = LNotConfigured
instance NotConfigurable ExplainConfig where notConfigured = ENotConfigured

type VanillaFilterConfigurable :: (* -> *) -> Constraint
class VanillaFilterConfigurable config where
  unexpected :: (a -> String) -> config a
  because :: (a -> String) -> config a
  unexpectedBecause :: (a -> String) -> (a -> String) -> config a

instance VanillaFilterConfigurable FilterConfig where
  unexpected = VSUnexpected
  because = VSBecause
  unexpectedBecause = VSUnexpectedBecause

instance VanillaFilterConfigurable VanillaFilterConfig where
  unexpected = VUnexpected
  because = VBecause
  unexpectedBecause = VUnexpectedBecause

type SpecializedFilterConfigurable :: (* -> *) -> Constraint
class SpecializedFilterConfigurable config where
  specializedFilter :: (a -> NonEmpty String) -> config a

instance SpecializedFilterConfigurable FilterConfig where
  specializedFilter = VSSpecializedFilter
instance SpecializedFilterConfigurable SpecializedFilterConfig where
  specializedFilter = SSpecializedFilter

type BasicFilterConfigurable :: (* -> *) -> Constraint
class BasicFilterConfigurable config where
  basicFilter :: config a

instance BasicFilterConfigurable FilterConfig where basicFilter = VSBasicFilter
instance BasicFilterConfigurable VanillaFilterConfig where basicFilter = VBasicFilter
instance BasicFilterConfigurable SpecializedFilterConfig where basicFilter = SBasicFilter

badCharsFail :: Map Char (NonEmpty String) -> VerifiedBadChars
badCharsFail = BadCharsFail
badCharsReason :: Map Char String -> VerifiedBadChars
badCharsReason = BadCharsReason

type Unverified :: * -> Constraint
class Unverified config where
  unverified :: config

instance Unverified VerifiedBadChars where unverified = BadCharsUnverified

singleton :: a -> NonEmpty a
singleton x = x :| []
