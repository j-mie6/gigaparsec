{-# LANGUAGE Safe #-}
{-# LANGUAGE NoMonomorphismRestriction, BlockArguments #-}
module Text.Gigaparsec.Token.Errors (
    ErrorConfig(
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
      filterNameIllFormedIdentifier, filterNameIllFormedOperator
    ),
    defaultErrorConfig,
    LabelWithExplainConfig, LabelWithExplainConfigurable(..),
    LabelConfig, LabelConfigurable(..),
    ExplainConfig, ExplainConfigurable(..),
    NotConfigurable(..),
    FilterConfig,
    VanillaFilterConfig, VanillaFilterConfigurable(..),
    SpecializedFilterConfig, SpecializedFilterConfigurable(..),
    BasicFilterConfigurable(..),
    Bits(B8, B16, B32, B64)
  ) where

import Data.Set (Set)
import Data.Kind (Constraint)
import Text.Gigaparsec.Internal.Token.Numeric (Bits(B8, B16, B32, B64))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

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
              , labelIntegerDecimalEnd :: Maybe Bits -> LabelConfig
              , labelIntegerHexadecimalEnd :: Maybe Bits -> LabelConfig
              , labelIntegerOctalEnd :: Maybe Bits -> LabelConfig
              , labelIntegerBinaryEnd :: Maybe Bits -> LabelConfig
              , labelIntegerNumberEnd :: Maybe Bits -> LabelConfig
              , filterIntegerOutOfBounds :: Integer -> Integer -> Int -> FilterConfig Integer
              , labelNameIdentifier :: String
              , labelNameOperator :: String
              , unexpectedNameIllegalIdentifier :: String -> String
              , unexpectedNameIllegalOperator :: String -> String
              , filterNameIllFormedIdentifier :: FilterConfig String
              , filterNameIllFormedOperator :: FilterConfig String
              }

{-
def labelCharAscii: LabelWithExplainConfig = NotConfigured
def labelCharLatin1: LabelWithExplainConfig = NotConfigured
def labelCharBasicMultilingualPlane: LabelWithExplainConfig = NotConfigured
def labelCharUtf16: LabelWithExplainConfig = NotConfigured
def labelCharAsciiEnd: LabelConfig = NotConfigured
def labelCharLatin1End: LabelConfig = NotConfigured
def labelCharBasicMultilingualPlaneEnd: LabelConfig = NotConfigured
def labelCharUtf16End: LabelConfig = NotConfigured
def labelStringAscii(@unused multi: Boolean, @unused raw: Boolean): LabelWithExplainConfig = NotConfigured
def labelStringLatin1(@unused multi: Boolean, @unused raw: Boolean): LabelWithExplainConfig = NotConfigured
def labelStringUtf16(@unused multi: Boolean, @unused raw: Boolean): LabelWithExplainConfig = NotConfigured
def labelStringAsciiEnd(@unused multi: Boolean, @unused raw: Boolean): LabelConfig = NotConfigured
def labelStringLatin1End(@unused multi: Boolean, @unused raw: Boolean): LabelConfig = NotConfigured
def labelStringUtf16End(@unused multi: Boolean, @unused raw: Boolean): LabelConfig = NotConfigured
def labelStringCharacter: LabelConfig = Label("string character")
def labelGraphicCharacter: LabelWithExplainConfig = Label("graphic character")
def labelEscapeSequence: LabelWithExplainConfig = Label("escape sequence") //different to "invalid escape sequence"!
def labelEscapeNumeric(@unused radix: Int): LabelWithExplainConfig = NotConfigured
def labelEscapeNumericEnd(@unused prefix: Char, @unused radix: Int): LabelWithExplainConfig = NotConfigured
def labelEscapeEnd: LabelWithExplainConfig = LabelAndReason("end of escape sequence", "invalid escape sequence")
def labelStringEscapeEmpty: LabelConfig = NotConfigured
def labelStringEscapeGap: LabelConfig = Label("string gap")
def labelStringEscapeGapEnd: LabelConfig = Label("end of string gap")
def filterCharNonBasicMultilingualPlane: VanillaFilterConfig[Int] = new Because[Int] {
        def reason(@unused x: Int) = "non-BMP character"
    }
def filterCharNonAscii: VanillaFilterConfig[Int] = new Because[Int] {
        def reason(@unused x: Int) = "non-ascii character"
    }
def filterCharNonLatin1: VanillaFilterConfig[Int] = new Because[Int] {
        def reason(@unused x: Int) = "non-latin1 character"
    }
def filterStringNonAscii: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder] {
        def message(@unused s: StringBuilder) = Seq("non-ascii characters in string literal, this is not allowed")
    }
def filterStringNonLatin1: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder] {
        def message(@unused s: StringBuilder) = Seq("non-latin1 characters in string literal, this is not allowed")
    }
def filterEscapeCharRequiresExactDigits(@unused radix: Int, needed: Seq[Int]): SpecialisedFilterConfig[Int] =
        new SpecialisedMessage[Int] {
            def message(got: Int) = {
                assume(needed.nonEmpty, "cannot be empty!")
                val Some(formatted) = parsley.errors.helpers.disjunct(needed.toList.map(_.toString), oxfordComma = true): @unchecked
                Seq(s"numeric escape requires $formatted digits, but only got $got")
            }
        }
def filterEscapeCharNumericSequenceIllegal(maxEscape: Int, radix: Int): SpecialisedFilterConfig[BigInt] =
        new SpecialisedMessage[BigInt] {
            def message(escapeChar: BigInt) = Seq(
                if (escapeChar > BigInt(maxEscape)) {
                    s"${escapeChar.toString(radix)} is greater than the maximum character value of ${BigInt(maxEscape).toString(radix)}"
                }
                else s"illegal unicode codepoint: ${escapeChar.toString(radix)}"
            )
        }
def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = Unverified
def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = Unverified
def labelSymbol: Map[String, LabelWithExplainConfig] = Map.empty
// To unify, or not to unify
private [parsley] def defaultSymbolKeyword: Labeller = Label
private [parsley] def defaultSymbolOperator: Labeller = Label
// Other?
private [parsley] def defaultSymbolPunctuation: Labeller = NotConfigured
def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"
def labelSpaceEndOfLineComment: LabelWithExplainConfig = Label("end of comment")
def labelSpaceEndOfMultiComment: LabelWithExplainConfig = Label("end of comment")
-}

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
        labelIntegerDecimalEnd = const notConfigured
        labelIntegerHexadecimalEnd = const notConfigured
        labelIntegerOctalEnd = const notConfigured
        labelIntegerBinaryEnd = const notConfigured
        labelIntegerNumberEnd = const notConfigured
        filterIntegerOutOfBounds small big nativeRadix = specializedFilter
          (outOfBounds small big nativeRadix)
        labelNameIdentifier = "identifier"
        labelNameOperator = "operator"
        unexpectedNameIllegalIdentifier = ("keyword " ++)
        unexpectedNameIllegalOperator = ("reserved operator " ++)
        filterNameIllFormedIdentifier = unexpected ("identifier " ++)
        filterNameIllFormedOperator = unexpected ("operator " ++)

outOfBounds :: Integer -> Integer -> Int -> Integer -> [String]
outOfBounds small big radix _n = [
    "literal is not within the range " ++ resign small (" to " ++ resign big "")
  ]
  where resign n
          | n < 0 = ('-' :) . showIntAtBase (toInteger radix) intToDigit (abs n)
          | otherwise = showIntAtBase (toInteger radix) intToDigit n


-- TODO: move to internal module, except the typeclasses
type LabelWithExplainConfig :: *
data LabelWithExplainConfig = LENotConfigured
                            | LELabel !(Set String)
                            | LEReason !String
                            | LEHidden
                            | LELabelAndReason !(Set String) !String

type LabelConfig :: *
data LabelConfig = LNotConfigured
                 | LLabel !(Set String)
                 | LHidden

type ExplainConfig :: *
data ExplainConfig = ENotConfigured
                   | EReason !String

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

type FilterConfig :: * -> *
data FilterConfig a = VSBasicFilter
                    | VSSpecializedFilter (a -> [String])
                    | VSUnexpected (a -> String)
                    | VSBecause (a -> String)
                    | VSUnexpectedBecause (a -> String) (a -> String)

type VanillaFilterConfig :: * -> *
data VanillaFilterConfig a = VBasicFilter
                           | VUnexpected (a -> String)
                           | VBecause (a -> String)
                           | VUnexpectedBecause (a -> String) (a -> String)

type SpecializedFilterConfig :: * -> *
data SpecializedFilterConfig a = SBasicFilter
                               | SSpecializedFilter (a -> [String])

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
  specializedFilter :: (a -> [String]) -> config a

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
