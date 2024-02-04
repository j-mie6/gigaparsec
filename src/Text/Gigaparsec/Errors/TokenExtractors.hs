{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
module Text.Gigaparsec.Errors.TokenExtractors (
    Token(..), TokenExtractor,
    tillNextWhitespace,
    singleChar,
    matchParserDemand--,
    --lexToken
  ) where

import Text.Gigaparsec (Parsec)

import Data.Char (generalCategory, ord, GeneralCategory(Format, Surrogate, PrivateUse, NotAssigned, Control))
import Data.Char qualified as Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (maximumBy)
import Numeric (showHex)
import Data.Function (on)

type TokenExtractor :: *
type TokenExtractor = NonEmpty Char -> Word -> Bool -> Token

{-|
This type represents an extracted token returned by 'unexpectedToken' in 'ErrorBuilder'.

There is deliberately no analogue for @EndOfInput@ because we guarantee that non-empty
residual input is provided to token extraction.
-}
type Token :: *
data Token = Raw                   -- ^ This is a token that is directly extracted from the residual input itself.
              !String              -- ^ the input extracted.
           | Named                 -- ^ This is a token that has been given a name, and is treated like a labelled item.
              !String              -- ^ the description of the token.
              {-# UNPACK #-} !Word -- ^ the amount of residual input this token ate.

{-# INLINABLE tillNextWhitespace #-}
-- TillNextWhitespace with matches parser demand
tillNextWhitespace :: Bool -> (Char -> Bool) -> TokenExtractor
tillNextWhitespace _ _ (whitespaceOrUnprintable -> Just tok) _ _ = tok
tillNextWhitespace trimToDemand isSpace (c :| cs) parserDemanded _
  | trimToDemand = Raw (take (fromIntegral parserDemanded) (tillSpace (c:cs)))
  | otherwise    = Raw (tillSpace (c:cs))
  where tillSpace = takeWhile (not . isSpace)

singleChar :: TokenExtractor
singleChar (whitespaceOrUnprintable -> Just tok) _ _ = tok
singleChar (c :| _) _ _ = Raw [c]

matchParserDemand :: TokenExtractor
matchParserDemand (whitespaceOrUnprintable -> Just tok) _ _ = tok
matchParserDemand (c :| cs) parserDemanded _ = Raw (take (fromIntegral parserDemanded) (c:cs))

whitespaceOrUnprintable :: NonEmpty Char -> Maybe Token
whitespaceOrUnprintable ('\n' :| _) = Just $ Named "newline" 1
whitespaceOrUnprintable ('\r' :| _) = Just $ Named "carriage return" 1
whitespaceOrUnprintable ('\t' :| _) = Just $ Named "tab" 1
whitespaceOrUnprintable (' ' :| _) = Just $ Named "space" 1
whitespaceOrUnprintable (c :| _)
  | Char.isSpace c = Just $ Named "whitespace character" 1
  | otherwise      = case generalCategory c of
    Format -> unprintable
    Surrogate -> unprintable
    PrivateUse -> unprintable
    NotAssigned -> unprintable
    Control -> unprintable
    _ -> Nothing
  where unprintable = Just $ Named ("non-printable character (\\x" ++ showHex (ord c) ")") 1

lexToken :: [Parsec String] -> TokenExtractor -> TokenExtractor
lexToken = lexTokenWithSelect (maximumBy (compare `on` snd))

lexTokenWithSelect :: (NonEmpty (String, Word) -> (String, Word)) -> [Parsec String] -> TokenExtractor -> TokenExtractor
lexTokenWithSelect = undefined