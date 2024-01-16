{-# LANGUAGE Safe #-}
-- TODO: refine, move to Internal
module Text.Gigaparsec.Token.Text (module Text.Gigaparsec.Token.Text) where

import Text.Gigaparsec (Parsec, void, (<|>), empty, filterS)
import Text.Gigaparsec.Char (char, digit, hexDigit, octDigit, bit, satisfy, trie)
import Text.Gigaparsec.Token.Descriptions (TextDesc(..), EscapeDesc(..), NumericEscape, CharPredicate)
import Text.Gigaparsec.Token.Generic (GenericNumeric(zeroAllowedDecimal, zeroAllowedHexadecimal, zeroAllowedOctal, zeroAllowedBinary))
import Data.Char (isSpace)
import Data.Map qualified as Map (insert, null, map)

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
    escMapped
      | Map.null escs = empty
      | otherwise     = trie (Map.map pure escs)

    numericEscape = decimalEsc <|> hexadecimalEsc <|> octalEsc <|> binaryEsc

    decimalEsc = fromDesc 10 decimalEscape (zeroAllowedDecimal gen) digit
    hexadecimalEsc = fromDesc 16 hexadecimalEscape (zeroAllowedHexadecimal gen) hexDigit
    octalEsc = fromDesc 8 octalEscape (zeroAllowedOctal gen) octDigit
    binaryEsc = fromDesc 2 binaryEscape (zeroAllowedBinary gen) bit

    fromDesc :: Int -> NumericEscape -> Parsec Integer -> Parsec Char -> Parsec Char
    fromDesc radix _ integer digit = undefined --TODO:

lexemeText :: (forall a. Parsec a -> Parsec a) -> TextParsers t -> TextParsers t
lexemeText lexe TextParsers{..} = TextParsers {
    unicode = lexe unicode,
    ascii = lexe ascii,
    latin1 = lexe latin1
  }
