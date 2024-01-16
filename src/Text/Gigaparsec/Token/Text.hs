{-# LANGUAGE Safe #-}
-- TODO: refine, move to Internal
module Text.Gigaparsec.Token.Text (module Text.Gigaparsec.Token.Text) where

import Text.Gigaparsec (Parsec, void, (<|>), empty, filterS, mapMaybeS, somel, (<~>), ($>), atomic, some)
import Text.Gigaparsec.Char (char, digit, hexDigit, octDigit, bit, satisfy, trie, string)
import Text.Gigaparsec.Token.Descriptions (TextDesc(..), EscapeDesc(..), NumericEscape (NumericSupported, NumericIllegal, numDigits, maxValue, prefix), CharPredicate, NumberOfDigits (Exactly, AtMost, Unbounded))
import Text.Gigaparsec.Token.Generic (GenericNumeric(zeroAllowedDecimal, zeroAllowedHexadecimal, zeroAllowedOctal, zeroAllowedBinary))
import Data.Char (isSpace, chr, ord, digitToInt, isAscii, isLatin1)
import Data.Map qualified as Map (insert, map)
import Data.Set (Set)
import Data.Set qualified as Set (toList)
import Data.List.NonEmpty (NonEmpty((:|)), sort)
import Text.Gigaparsec.Registers (Reg, make, unsafeMake, gets, modify, put, get)
import Text.Gigaparsec.Combinator (guardS, choice, manyTill)
import Control.Applicative (liftA3)
import Data.Maybe (catMaybes)

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

type StringChar :: *
data StringChar = RawChar
                | EscapeChar {-# UNPACK #-} !Char (Parsec (Maybe Char))

mkEscapeChar :: EscapeDesc -> Escape -> Parsec () -> StringChar
mkEscapeChar !desc !esc !space = EscapeChar (escBegin desc) stringEsc
  where stringEsc = escapeBegin esc *> (escapeGap $> Nothing <|>
                                        escapeEmpty $> Nothing <|>
                                        Just <$> escapeCode esc)
        escapeEmpty = maybe empty char (emptyEscape desc)
        escapeGap
          | gapsSupported desc = some space *> escapeBegin esc
          | otherwise = empty

mkChar :: StringChar -> CharPredicate -> Parsec (Maybe Char)
mkChar RawChar = maybe empty (fmap Just . satisfy)
mkChar (EscapeChar escBegin stringEsc) =
  foldr (\p -> (<|> fmap Just (satisfy (\c -> p c && c /= escBegin)))) stringEsc

isRawChar :: StringChar -> Bool
isRawChar RawChar = True
isRawChar EscapeChar{} = False

ensureAscii :: Parsec String -> Parsec String
ensureAscii = filterS (all isAscii)

ensureLatin1 :: Parsec String -> Parsec String
ensureLatin1 = filterS (all isLatin1)

mkStringParsers :: Set (String, String) -> StringChar -> CharPredicate -> Bool -> StringParsers
mkStringParsers !ends !stringChar !isGraphic !allowsAllSpace = TextParsers {..}
  where ascii = stringLiteral ensureAscii
        latin1 = stringLiteral ensureLatin1
        unicode = stringLiteral id

        stringLiteral :: (Parsec String -> Parsec String) -> Parsec String
        stringLiteral valid = choice (map (uncurry (makeStringParser valid)) (Set.toList ends))

        makeStringParser :: (Parsec String -> Parsec String) -> String -> String -> Parsec String
        makeStringParser valid begin end@(terminalInit : _) =
          let strChar = mkChar stringChar (letter terminalInit allowsAllSpace isGraphic)
          in (string begin *>) . valid $
               catMaybes <$> manyTill (Just <$> char terminalInit <|> strChar) (atomic (string end))
        makeStringParser _ _ [] = error "string terminals cannot be empty"

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

    atMost' :: Int -> Parsec Char -> Reg r Word -> Parsec Integer
    atMost' radix dig atMostR =
      -- FIXME: surely this is an inefficient mess with the translations?
      somel (\n d -> n * toInteger radix + toInteger (digitToInt d)) 0
            (guardS (gets atMostR (> 0)) *> dig <* modify atMostR pred)

    atMost :: Word -> Int -> Parsec Char -> Parsec Integer
    atMost n radix dig = make n (atMost' radix dig)

    exactly :: Word -> Word -> Int -> Parsec Char -> NonEmpty Word -> Parsec Integer
    exactly n full radix dig _reqDigits = make n $ \atMostR ->
      mapMaybeS (\(num, m) -> if m == full then Just num else Nothing)
                (atMost' radix dig atMostR <~> gets atMostR (full -))

    oneOfExactly' :: NonEmpty Word -> Word -> Word -> [Word] -> Int -> Parsec Char -> Reg r Word -> Parsec Integer
    oneOfExactly' reqDigits digits m [] radix dig digitsParsed =
      exactly digits m radix dig reqDigits <* put digitsParsed digits
    oneOfExactly' reqDigits digits m (n:ns) radix dig digitsParsed =
      let theseDigits = exactly digits m radix dig reqDigits
          restDigits =
                atomic (Just <$> oneOfExactly' reqDigits (n - m) n ns radix dig digitsParsed
                     <* modify digitsParsed (+ digits))
            <|> put digitsParsed digits $> Nothing
          combine !x Nothing !_ = x
          -- digits is removed here, because it's been added before the get
          combine x (Just y) e = x * toInteger radix ^ (e - digits) + y
      in liftA3 combine theseDigits restDigits (get digitsParsed)

    oneOfExactly :: NonEmpty Word -> Int -> Parsec Char -> Parsec Integer
    oneOfExactly ns radix dig =
      let reqDigits@(m :| ms) = sort ns
      in unsafeMake (oneOfExactly' reqDigits m m ms radix dig)

    fromDesc :: Int -> NumericEscape -> Parsec Integer -> Parsec Char -> Parsec Char
    fromDesc !_ NumericIllegal !_ !_ = empty
    fromDesc radix NumericSupported{..} integer dig = case numDigits of
      Unbounded  -> boundedChar integer maxValue prefix radix
      AtMost n   -> boundedChar (atMost n radix dig) maxValue prefix radix
      Exactly ns -> boundedChar (oneOfExactly ns radix dig) maxValue prefix radix

lexemeText :: (forall a. Parsec a -> Parsec a) -> TextParsers t -> TextParsers t
lexemeText lexe TextParsers{..} = TextParsers {
    unicode = lexe unicode,
    ascii = lexe ascii,
    latin1 = lexe latin1
  }
