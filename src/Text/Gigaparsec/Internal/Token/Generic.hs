{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists, NamedFieldPuns #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Generic (module Text.Gigaparsec.Internal.Token.Generic) where

import Text.Gigaparsec ((<|>), many, Parsec, ($>), (<:>))
import Text.Gigaparsec.Char (satisfy, char, digit, hexDigit, octDigit, bit)
import Text.Gigaparsec.Combinator (optional)
import Text.Gigaparsec.Errors.Combinator ((<?>), hide)
import Text.Gigaparsec.Token.Descriptions (
    BreakCharDesc(BreakCharSupported, NoBreakChar),
    NumericDesc(NumericDesc, literalBreakChar, leadingZerosAllowed)
  )

import Data.Char (isDigit, isHexDigit, isOctDigit, digitToInt)
import Data.List (foldl')
import Text.Gigaparsec.Token.Errors (ErrorConfig (labelNumericBreakChar))
import Text.Gigaparsec.Internal.Token.Errors (annotate, LabelConfig)

type GenericNumeric :: *
data GenericNumeric = Generic { zeroAllowedDecimal :: LabelConfig -> Parsec Integer
                              , zeroAllowedHexadecimal :: LabelConfig -> Parsec Integer
                              , zeroAllowedOctal :: LabelConfig -> Parsec Integer
                              , zeroAllowedBinary :: LabelConfig -> Parsec Integer
                              , zeroNotAllowedDecimal :: LabelConfig -> Parsec Integer
                              , zeroNotAllowedHexadecimal :: LabelConfig -> Parsec Integer
                              , zeroNotAllowedOctal :: LabelConfig -> Parsec Integer
                              , zeroNotAllowedBinary :: LabelConfig -> Parsec Integer
                              , plainDecimal :: NumericDesc -> LabelConfig -> Parsec Integer
                              , plainHexadecimal :: NumericDesc -> LabelConfig -> Parsec Integer
                              , plainOctal :: NumericDesc -> LabelConfig -> Parsec Integer
                              , plainBinary :: NumericDesc -> LabelConfig -> Parsec Integer
                              }

mkGeneric :: ErrorConfig -> GenericNumeric
mkGeneric !err = Generic {..}
  where ofRadix1 :: Integer -> Parsec Char -> LabelConfig -> Parsec Integer
        ofRadix1 radix dig = ofRadix2 radix dig dig
        ofRadix2 :: Integer -> Parsec Char -> Parsec Char -> LabelConfig -> Parsec Integer
        ofRadix2 radix startDig dig label =
          foldl' (withDigit radix) 0 <$> (startDig <:> many (annotate label dig)) --TODO: improve

        ofRadixBreak1 :: Integer -> Parsec Char -> Char -> LabelConfig -> Parsec Integer
        ofRadixBreak1 radix dig = ofRadixBreak2 radix dig dig
        ofRadixBreak2 :: Integer -> Parsec Char -> Parsec Char -> Char -> LabelConfig -> Parsec Integer
        ofRadixBreak2 radix startDig dig breakChar label =
          foldl' (withDigit radix) 0 <$> (startDig <:> many (optional (annotate (labelNumericBreakChar err) (annotate label (char breakChar))) *> annotate label dig)) --TODO: improve

        nonZeroDigit = satisfy (\c -> isDigit c && c /= '0') <?> ["digit"]
        nonZeroHexDigit = satisfy (\c -> isHexDigit c && c /= '0') <?> ["hexadecimal digit"]
        nonZeroOctDigit = satisfy (\c -> isOctDigit c && c /= '0') <?> ["octal digit"]
        nonZeroBit = char '1' <?> ["bit"]
        -- why secret? so that the above digits can be marked as digits without "non-zero or zero digit"
        secretZero :: Parsec Integer
        secretZero = hide (char '0') $> 0

        zeroAllowedDecimal = ofRadix1 10 digit
        zeroAllowedHexadecimal = ofRadix1 16 hexDigit
        zeroAllowedOctal = ofRadix1 8 octDigit
        zeroAllowedBinary = ofRadix1 2 bit

        zeroNotAllowedDecimal label = ofRadix2 10 nonZeroDigit digit label <|> secretZero
        zeroNotAllowedHexadecimal label = ofRadix2 16 nonZeroHexDigit hexDigit label <|> secretZero
        zeroNotAllowedOctal label = ofRadix2 8 nonZeroOctDigit octDigit label <|> secretZero
        zeroNotAllowedBinary label = ofRadix2 2 nonZeroBit bit label <|> secretZero

        plainDecimal NumericDesc{leadingZerosAllowed, literalBreakChar} label = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal label
          NoBreakChar                                  -> zeroNotAllowedDecimal label
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 10 digit c label
          BreakCharSupported c _                       -> ofRadixBreak2 10 nonZeroDigit digit c label <|> secretZero

        plainHexadecimal NumericDesc{leadingZerosAllowed, literalBreakChar} label = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal label
          NoBreakChar                                  -> zeroNotAllowedDecimal label
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 16 hexDigit c label
          BreakCharSupported c _                       -> ofRadixBreak2 16 nonZeroHexDigit hexDigit c label <|> secretZero

        plainOctal NumericDesc{leadingZerosAllowed, literalBreakChar} label = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal label
          NoBreakChar                                  -> zeroNotAllowedDecimal label
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 8 octDigit c label
          BreakCharSupported c _                       -> ofRadixBreak2 8 nonZeroOctDigit octDigit c label <|> secretZero

        plainBinary NumericDesc{leadingZerosAllowed, literalBreakChar} label = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal label
          NoBreakChar                                  -> zeroNotAllowedDecimal label
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 2 bit c label
          BreakCharSupported c _                       -> ofRadixBreak2 2 nonZeroBit bit c label <|> secretZero

withDigit :: Integer -> Integer -> Char -> Integer
withDigit radix n d = n * radix + fromIntegral (digitToInt d)
