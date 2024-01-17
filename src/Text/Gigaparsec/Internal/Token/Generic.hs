{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists, NamedFieldPuns #-}
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

type GenericNumeric :: *
data GenericNumeric = Generic { zeroAllowedDecimal :: Parsec Integer
                              , zeroAllowedHexadecimal :: Parsec Integer
                              , zeroAllowedOctal :: Parsec Integer
                              , zeroAllowedBinary :: Parsec Integer
                              , zeroNotAllowedDecimal :: Parsec Integer
                              , zeroNotAllowedHexadecimal :: Parsec Integer
                              , zeroNotAllowedOctal :: Parsec Integer
                              , zeroNotAllowedBinary :: Parsec Integer
                              , plainDecimal :: NumericDesc -> Parsec Integer
                              , plainHexadecimal :: NumericDesc -> Parsec Integer
                              , plainOctal :: NumericDesc -> Parsec Integer
                              , plainBinary :: NumericDesc -> Parsec Integer
                              }

mkGeneric :: GenericNumeric
mkGeneric = Generic {..}
  where ofRadix1 :: Integer -> Parsec Char -> Parsec Integer
        ofRadix1 radix dig = ofRadix2 radix dig dig
        ofRadix2 :: Integer -> Parsec Char -> Parsec Char -> Parsec Integer
        ofRadix2 radix startDig dig = foldl' (withDigit radix) 0 <$> (startDig <:> many dig) --TODO: improve

        ofRadixBreak1 :: Integer -> Parsec Char -> Char -> Parsec Integer
        ofRadixBreak1 radix dig = ofRadixBreak2 radix dig dig
        ofRadixBreak2 :: Integer -> Parsec Char -> Parsec Char -> Char -> Parsec Integer
        ofRadixBreak2 radix startDig dig breakChar =
          foldl' (withDigit radix) 0 <$> (startDig <:> many (optional (char breakChar) *> dig)) --TODO: improve

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

        zeroNotAllowedDecimal = ofRadix2 10 nonZeroDigit digit <|> secretZero
        zeroNotAllowedHexadecimal = ofRadix2 16 nonZeroHexDigit hexDigit <|> secretZero
        zeroNotAllowedOctal = ofRadix2 8 nonZeroOctDigit octDigit <|> secretZero
        zeroNotAllowedBinary = ofRadix2 2 nonZeroBit bit <|> secretZero

        plainDecimal NumericDesc{leadingZerosAllowed, literalBreakChar} = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal
          NoBreakChar                                  -> zeroNotAllowedDecimal
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 10 digit c
          BreakCharSupported c _                       -> ofRadixBreak2 10 nonZeroDigit digit c <|> secretZero

        plainHexadecimal NumericDesc{leadingZerosAllowed, literalBreakChar} = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal
          NoBreakChar                                  -> zeroNotAllowedDecimal
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 16 hexDigit c
          BreakCharSupported c _                       -> ofRadixBreak2 16 nonZeroHexDigit hexDigit c <|> secretZero

        plainOctal NumericDesc{leadingZerosAllowed, literalBreakChar} = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal
          NoBreakChar                                  -> zeroNotAllowedDecimal
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 8 octDigit c
          BreakCharSupported c _                       -> ofRadixBreak2 8 nonZeroOctDigit octDigit c <|> secretZero

        plainBinary NumericDesc{leadingZerosAllowed, literalBreakChar} = case literalBreakChar of
          NoBreakChar | leadingZerosAllowed            -> zeroAllowedDecimal
          NoBreakChar                                  -> zeroNotAllowedDecimal
          BreakCharSupported c _ | leadingZerosAllowed -> ofRadixBreak1 2 bit c
          BreakCharSupported c _                       -> ofRadixBreak2 2 nonZeroBit bit c <|> secretZero

withDigit :: Integer -> Integer -> Char -> Integer
withDigit radix n d = n * radix + fromIntegral (digitToInt d)
