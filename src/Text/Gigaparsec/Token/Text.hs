{-# LANGUAGE Safe #-}
-- TODO: refine, move to Internal
module Text.Gigaparsec.Token.Text (module Text.Gigaparsec.Token.Text) where

import Text.Gigaparsec (Parsec, void, (<|>), empty, filterS, mapMaybeS)
import Text.Gigaparsec.Char (char, digit, hexDigit, octDigit, bit, satisfy, trie)
import Text.Gigaparsec.Token.Descriptions (TextDesc(..), EscapeDesc(..), NumericEscape (NumericSupported, NumericIllegal, numDigits, maxValue, prefix), CharPredicate, NumberOfDigits (Exactly, AtMost, Unbounded))
import Text.Gigaparsec.Token.Generic (GenericNumeric(zeroAllowedDecimal, zeroAllowedHexadecimal, zeroAllowedOctal, zeroAllowedBinary))
import Data.Char (isSpace, chr, ord)
import Data.Map qualified as Map (insert, map)
import Data.List.NonEmpty (NonEmpty((:|)))

type TextParsers :: * -> *
data TextParsers t = TextParsers { unicode :: Parsec t
                                 , ascii :: Parsec t
                                 , latin1 :: Parsec t
                                 }

-- I want the convenient naming, sue me
type StringParsers :: *
type StringParsers = TextParsers String

type CharacterParsers :: *
type CharacterParsers = TextParsers Char

mkCharacterParsers :: TextDesc -> Escape -> CharacterParsers
mkCharacterParsers TextDesc{..} escape = TextParsers {..}
  where unicode = lit uncheckedUniLetter
        ascii = lit (filterS (<= '\x7f') uncheckedUniLetter)
        latin1 = lit (filterS (<= '\xff') uncheckedUniLetter)

        quote = char characterLiteralEnd
        lit c = quote *> c <* quote
        uncheckedUniLetter = escapeChar escape <|> graphic

        graphic = maybe empty satisfy (letter characterLiteralEnd False graphicCharacter)

letter :: Char -> Bool -> CharPredicate -> CharPredicate
letter !terminalLead !allowsAllSpace (Just g)
  | allowsAllSpace = Just $ \c -> c /= terminalLead && (g c || isSpace c)
  | otherwise      = Just $ \c -> c /= terminalLead && g c
letter _ _ Nothing = Nothing

type Escape :: *
data Escape = Escape { escapeCode :: !(Parsec Char)
                     , escapeBegin :: !(Parsec ())
                     , escapeChar :: !(Parsec Char)
                     }

mkEscape :: EscapeDesc -> GenericNumeric -> Escape
mkEscape EscapeDesc{..} gen = Escape {..}
  where
    escapeBegin = void (char escBegin)
    escapeCode = escMapped <|> numericEscape
    escapeChar = escapeBegin *> escapeCode

    escs = foldr (\c -> Map.insert [c] c) mapping literals
    escMapped = trie (Map.map pure escs)

    numericEscape = decimalEsc <|> hexadecimalEsc <|> octalEsc <|> binaryEsc

    decimalEsc = fromDesc 10 decimalEscape (zeroAllowedDecimal gen) digit
    hexadecimalEsc = fromDesc 16 hexadecimalEscape (zeroAllowedHexadecimal gen) hexDigit
    octalEsc = fromDesc 8 octalEscape (zeroAllowedOctal gen) octDigit
    binaryEsc = fromDesc 2 binaryEscape (zeroAllowedBinary gen) bit

    boundedChar :: Parsec Integer -> Char -> Maybe Char -> Int -> Parsec Char
    boundedChar p maxValue prefix _radix = foldr (\c t -> char c *> t) (mapMaybeS f p) prefix
      where f c
             | c < toInteger (ord maxValue) = Just (chr (fromInteger c))
             | otherwise = Nothing

    atMost :: Word -> Int -> Parsec Char -> Parsec Integer
    atMost _ _ _ = empty -- TODO:

    oneOfExactly :: Word -> [Word] -> Int -> Parsec Char -> Parsec Integer
    oneOfExactly _ _ _ _ = empty --TODO:

    fromDesc :: Int -> NumericEscape -> Parsec Integer -> Parsec Char -> Parsec Char
    fromDesc !_ NumericIllegal !_ !_ = empty
    fromDesc radix NumericSupported{..} integer dig = case numDigits of
      Unbounded -> boundedChar integer maxValue prefix radix
      AtMost n -> boundedChar (atMost n radix dig) maxValue prefix radix
      Exactly (n :| ns) -> boundedChar (oneOfExactly n ns radix digit) maxValue prefix radix

lexemeText :: (forall a. Parsec a -> Parsec a) -> TextParsers t -> TextParsers t
lexemeText lexe TextParsers{..} = TextParsers {
    unicode = lexe unicode,
    ascii = lexe ascii,
    latin1 = lexe latin1
  }
