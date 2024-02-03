{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Text (module Text.Gigaparsec.Internal.Token.Text) where

import Text.Gigaparsec (Parsec, void, (<|>), empty, somel, (<~>), ($>), atomic, some)
import Text.Gigaparsec.Char (char, digit, hexDigit, octDigit, bit, satisfy, trie, string)
import Text.Gigaparsec.Token.Descriptions (
    TextDesc(TextDesc, characterLiteralEnd, graphicCharacter),
    EscapeDesc(EscapeDesc, escBegin, emptyEscape, gapsSupported, mapping, literals,
               decimalEscape, hexadecimalEscape, octalEscape, binaryEscape),
    NumericEscape(NumericSupported, NumericIllegal, numDigits, maxValue, prefix),
    CharPredicate,
    NumberOfDigits(Exactly, AtMost, Unbounded)
  )
import Text.Gigaparsec.Token.Errors (
    ErrorConfig(verifiedCharBadCharsUsedInLiteral, verifiedStringBadCharsUsedInLiteral,
                filterCharNonAscii, filterCharNonLatin1,
                labelCharAscii, labelCharAsciiEnd, labelCharLatin1, labelCharLatin1End,
                labelCharUnicodeEnd, labelCharUnicode,
                labelGraphicCharacter, labelStringCharacter,
                filterStringNonAscii, filterStringNonLatin1,
                labelEscapeEnd, labelEscapeSequence, filterEscapeCharNumericSequenceIllegal,
                filterEscapeCharRequiresExactDigits, labelEscapeNumericEnd, labelEscapeNumeric,
                labelStringEscapeGap, labelStringEscapeGapEnd, labelStringEscapeEmpty,
                labelStringAscii, labelStringAsciiEnd, labelStringLatin1, labelStringLatin1End,
                labelStringUnicode, labelStringUnicodeEnd),
    NotConfigurable (notConfigured)
  )
import Text.Gigaparsec.Internal.Token.Errors (
    checkBadChar, filterS, annotate, mapMaybeS, mapMaybeS',
    LabelWithExplainConfig, LabelConfig
  )
import Text.Gigaparsec.Internal.Token.Generic (
    GenericNumeric(zeroAllowedDecimal, zeroAllowedHexadecimal, zeroAllowedOctal, zeroAllowedBinary)
  )
import Data.Char (isSpace, chr, ord, digitToInt, isAscii, isLatin1)
import Data.Map qualified as Map (insert, map)
import Data.Set (Set)
import Data.Set qualified as Set (toList)
import Data.List.NonEmpty (NonEmpty((:|)), sort)
import Text.Gigaparsec.Registers (Reg, make, unsafeMake, gets, modify, put, get)
import Text.Gigaparsec.Combinator (guardS, choice, manyTill)
import Control.Applicative (liftA3)
import Data.Maybe (catMaybes)

-- TODO: is it possible to /actually/ support Text/Bytestring in future?
-- Perhaps something like the Numeric stuff?
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

mkCharacterParsers :: TextDesc -> Escape -> ErrorConfig -> CharacterParsers
mkCharacterParsers TextDesc{..} escape !err = TextParsers {..}
  where unicode = lit (labelCharUnicode err) (labelCharUnicodeEnd err) uncheckedUniLetter
        ascii = lit (labelCharAscii err) (labelCharAsciiEnd err) (filterS (filterCharNonAscii err) (> '\x7f') uncheckedUniLetter)
        latin1 = lit (labelCharLatin1 err) (labelCharLatin1End err) (filterS (filterCharNonLatin1 err) (> '\xff') uncheckedUniLetter)

        quote = char characterLiteralEnd
        lit label endLabel c = annotate label quote *> c <* annotate endLabel quote
        uncheckedUniLetter = escapeChar escape <|> graphic <|> checkBadChar (verifiedCharBadCharsUsedInLiteral err)

        graphic = annotate (labelGraphicCharacter err) $ maybe empty satisfy (letter characterLiteralEnd False graphicCharacter)

type StringChar :: *
data StringChar = RawChar
                | EscapeChar {-# UNPACK #-} !Char (Parsec (Maybe Char))

mkEscapeChar :: EscapeDesc -> Escape -> Parsec () -> ErrorConfig -> StringChar
mkEscapeChar !desc !esc !space !err = EscapeChar (escBegin desc) stringEsc
  where stringEsc = escapeBegin esc *> (escapeGap $> Nothing <|>
                                        escapeEmpty $> Nothing <|>
                                        Just <$> escapeCode esc)
        escapeEmpty = maybe empty (annotate (labelStringEscapeEmpty err) . char) (emptyEscape desc)
        escapeGap
          | gapsSupported desc = some (annotate (labelStringEscapeGap err) space)
                              *> annotate (labelStringEscapeGapEnd err) (escapeBegin esc)
          | otherwise = empty

mkChar :: StringChar -> ErrorConfig -> CharPredicate -> Parsec (Maybe Char)
mkChar RawChar !err = maybe empty ((<|> checkBadChar (verifiedStringBadCharsUsedInLiteral err)) . fmap Just . annotate (labelStringCharacter err) . satisfy)
mkChar (EscapeChar escBegin stringEsc) err =
  foldr (\p -> annotate (labelStringCharacter err) . (<|> checkBadChar (verifiedStringBadCharsUsedInLiteral err)) . (<|> fmap Just (annotate (labelGraphicCharacter err) (satisfy (\c -> p c && c /= escBegin)))))
        stringEsc

isRawChar :: StringChar -> Bool
isRawChar RawChar = True
isRawChar EscapeChar{} = False

ensureAscii :: ErrorConfig -> Parsec String -> Parsec String
ensureAscii !err = filterS (filterStringNonAscii err) (not . all isAscii)

ensureLatin1 :: ErrorConfig -> Parsec String -> Parsec String
ensureLatin1 !err = filterS (filterStringNonLatin1 err) (not . all isLatin1)

mkStringParsers :: Set (String, String) -> StringChar -> CharPredicate -> Bool -> ErrorConfig -> StringParsers
mkStringParsers !ends !stringChar !isGraphic !allowsAllSpace !err = TextParsers {..}
  where ascii = stringLiteral (ensureAscii err) (labelStringAscii err) (labelStringAsciiEnd err)
        latin1 = stringLiteral (ensureLatin1 err) (labelStringLatin1 err) (labelStringLatin1End err)
        unicode = stringLiteral id (labelStringUnicode err) (labelStringUnicodeEnd err)

        stringLiteral :: (Parsec String -> Parsec String)
                      -> (Bool -> Bool -> LabelWithExplainConfig)
                      -> (Bool -> Bool -> LabelConfig)
                      -> Parsec String
        stringLiteral valid openLabel closeLabel =
          choice (map (uncurry (makeStringParser valid openLabel closeLabel)) (Set.toList ends))

        makeStringParser :: (Parsec String -> Parsec String)
                         -> (Bool -> Bool -> LabelWithExplainConfig)
                         -> (Bool -> Bool -> LabelConfig)
                         -> String -> String -> Parsec String
        makeStringParser valid openLabel closeLabel begin end@(terminalInit : _) =
          let strChar = mkChar stringChar err (letter terminalInit allowsAllSpace isGraphic)
          in (annotate (openLabel allowsAllSpace (isRawChar stringChar)) (string begin) *>) . valid $
               catMaybes <$> manyTill (Just <$> char terminalInit <|> strChar)
                                      (annotate (closeLabel allowsAllSpace (isRawChar stringChar)) (atomic (string end)))
        makeStringParser _ _ _ _ [] = error "string terminals cannot be empty"

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

mkEscape :: EscapeDesc -> GenericNumeric -> ErrorConfig -> Escape
mkEscape EscapeDesc{..} gen !err = Escape {..}
  where
    escapeBegin = annotate (labelEscapeSequence err) $ void (char escBegin)
    escapeCode = annotate (labelEscapeEnd err) $
      escMapped <|> numericEscape
    escapeChar = escapeBegin *> escapeCode

    escs = foldr (\c -> Map.insert [c] c) mapping literals
    escMapped = trie (Map.map pure escs)

    numericEscape = decimalEsc <|> hexadecimalEsc <|> octalEsc <|> binaryEsc

    decimalEsc = fromDesc 10 decimalEscape (zeroAllowedDecimal gen notConfigured) digit
    hexadecimalEsc = fromDesc 16 hexadecimalEscape (zeroAllowedHexadecimal gen notConfigured) hexDigit
    octalEsc = fromDesc 8 octalEscape (zeroAllowedOctal gen notConfigured) octDigit
    binaryEsc = fromDesc 2 binaryEscape (zeroAllowedBinary gen notConfigured) bit

    boundedChar :: Parsec Integer -> Char -> Maybe Char -> Int -> Parsec Char
    boundedChar p maxValue prefix radix = annotate (labelEscapeNumeric err radix) $
      foldr (\c t -> char c *> annotate (labelEscapeNumericEnd err c radix) t)
            (mapMaybeS config f p)
            prefix
      where config = filterEscapeCharNumericSequenceIllegal err maxValue radix
            f c
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
    exactly n full radix dig reqDigits = make n $ \atMostR ->
      mapMaybeS' snd (filterEscapeCharRequiresExactDigits err radix reqDigits)
                 (\(num, m) -> if m == full then Just num else Nothing)
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

lexeme :: (forall a. Parsec a -> Parsec a) -> TextParsers t -> TextParsers t
lexeme lexe TextParsers{..} = TextParsers {
    unicode = lexe unicode,
    ascii = lexe ascii,
    latin1 = lexe latin1
  }
