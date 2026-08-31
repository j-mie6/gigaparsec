{-# LANGUAGE 
      OverloadedLists
    , TemplateHaskell
#-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JavascriptBench.Gigaparsec.Configured.Lexer where

import Control.Applicative (Alternative, (<**>))
import Data.Char (readLitChar, isSpace, digitToInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Read (readMaybe)

import Text.Gigaparsec (some, somel, empty)
import Text.Gigaparsec.Char (oneOf, char)
import Text.Gigaparsec.Combinator (fromMaybeS)
import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer (Lexer)
import Text.Gigaparsec.Token.Lexer qualified as L

import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Internal.TH.VersionAgnostic (Extension(TemplateHaskell))
import Text.Gigaparsec.Token.Patterns (IntegerParserConfig, lexerCombinators, lexerCombinatorsWithNames, overloadedStrings, emptyIntegerParserConfig)

import JavascriptBench.Shared (jsIdentStart, jsIdentLetter, jsKeywords, jsStringLetter)
import JavascriptBench.Gigaparsec.Configured.LexerIntCfg (jsIntCfg)




lexer :: L.Lexer
lexer = L.mkLexer lexicalDesc


lexicalDesc :: D.LexicalDesc
lexicalDesc = D.plain {
    D.nameDesc = nameDesc
  , D.symbolDesc = symbolDesc
  , D.textDesc = textDesc
  , D.spaceDesc = spaceDesc
  }

-------------------------------------------------------------------------------
-- Name Description

nameDesc :: D.NameDesc
nameDesc = D.plainName {
    D.identifierStart  = Just jsIdentStart
  , D.identifierLetter = Just jsIdentLetter
  }


-------------------------------------------------------------------------------
-- Whitespace Description

spaceDesc :: D.SpaceDesc
spaceDesc = D.plainSpace {
    D.space = Just isSpace
  , D.whitespaceIsContextDependent = False
  , D.lineCommentStart = "//"
  , D.multiLineCommentStart = "/*"
  , D.multiLineCommentEnd = "*/"
  , D.lineCommentAllowsEOF = True
  }

-------------------------------------------------------------------------------
-- Symbol Description

symbolDesc :: D.SymbolDesc
symbolDesc = D.plainSymbol {
    D.hardKeywords = jsKeywords
  , D.hardOperators = jsOperators
  , D.caseSensitive = True
  }

jsOperators :: Set String
jsOperators = [
  -- Arithmetic
    "--", "++", "-", "+"
  , "*", "/", "%"
  -- Bitwise
  , ">>", "<<", "&", "^"
  , "|", "~"
  -- Boolean
  , "!", "&&", "||"
  -- Comparators
  , "<=", ">=", "<", ">"
  , "==", "!="
  -- Assignment
  , "="
  ]

-------------------------------------------------------------------------------
-- Text (Char/String) Description

textDesc :: D.TextDesc
textDesc = D.plainText {
    D.escapeSequences = escapeDesc
  , D.characterLiteralEnd = '\''
  , D.stringEnds = [("\"", "\"")]
  -- As of ES6
  , D.multiStringEnds = [("`", "`")]
  , D.graphicCharacter = Just jsStringLetter
  }

-------------------------------------------------------------------------------
-- Escape Sequence Description

escapeDesc :: D.EscapeDesc
escapeDesc = D.plainEscape {
    D.escBegin = '\\'
  , D.literals = escapeSingleCharLiterals
  , D.mapping  = escapeMultiCharSequenceMap
  }

escapeSingleCharLiterals :: Set Char
escapeSingleCharLiterals = [
    'a', 'b' , 'f', 'n' , 't'
  , 'v', '\\', '"', '\'', '^'
  ]

escapeMultiCharSequenceMap :: Map String Char
escapeMultiCharSequenceMap = Map.fromList $ zip sequences asLiterals
  where
    asLiterals = map fst $ concatMap (readLitChar . ('\\' :)) sequences

    sequences :: [String]
    sequences = [
        "ACK", "BS" , "BEL", "CR" , "CAN", "DC1", "DC2", "DC3"
      , "DC4", "DEL", "DLE", "EM" , "ETX", "ETB", "ESC", "EOT"
      , "ENQ", "FF" , "FS" , "GS" , "HT" , "LF" , "NUL", "NAK"
      , "RS" , "SO" , "SOH", "SI" , "SP" , "STX", "SYN", "SUB"
      , "US" , "VT"
      ]

$(lexerCombinators [| lexer |] [
    'L.fully
  , 'L.identifier
  , 'L.stringLiteral
  , 'L.multiStringLiteral
  , 'L.charLiteral
  ])

$(lexerCombinatorsWithNames [| lexer |] [
    ('L.softKeyword, "keyword")
  , ('L.softOperator, "operator")
  , ('L.sym, "symbol")
  , ('L.whiteSpace, "whitespace")
  ])

$(overloadedStrings [| lexer |])

string :: Parsec String
string = L.ascii stringLiteral
    <|>  L.ascii multiStringLiteral


-------------------------------------------------------------------------------
-- Numeric Tokens
-- TODO: actually use the lexerDescription for this

naturalOrFloat :: Parsec (Either Int Double)
naturalOrFloat = natFloat <* whitespace

natFloat :: Parsec (Either Int Double)
natFloat = char '0' *> zeroNumFloat <|> decimalFloat

zeroNumFloat :: Parsec (Either Int Double)
zeroNumFloat = 
      Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> fromMaybeS empty (fractFloat <*> pure 0)
  <|> pure (Left 0)

decimalFloat :: Parsec (Either Int Double)
decimalFloat = fromMaybeS empty (decimal <**> (option' (Just . Left) fractFloat))

fractFloat :: Parsec (Int -> Maybe (Either Int Double))
fractFloat = f <$> fractExponent
  where
    f g x = fmap Right (g x)

fractExponent :: Parsec (Int -> Maybe Double)
fractExponent = 
      f <$> fraction <*> option' "" exponent'
  <|> f <$> pure "" <*> exponent'
  where
    f fract exp n = readMaybe (show n ++ fract ++ exp)

fraction :: Parsec [Char]
fraction = ('.' :) <$> (char '.'
        *> some (oneOf ['0'..'9']))

exponent' :: Parsec [Char]
exponent' = 
  ('e' :) <$> (oneOf (Set.fromList "eE")
          *> ((((:) <$> oneOf (Set.fromList "+-")) <|> pure id)
          <*> (show <$> decimal)))

decimal :: Parsec Int
decimal = number 10 (oneOf ['0'..'9'])

hexadecimal :: Parsec Int
hexadecimal = oneOf (Set.fromList "xX") *> number 16 (oneOf (['a'..'f'] <> ['A'..'F'] <> ['0'..'9']))

octal :: Parsec Int
octal = oneOf (Set.fromList "oO") *> number 8 (oneOf ['0'..'7'])

number :: Int -> Parsec Char -> Parsec Int
number base = somel (\x d -> base * x + digitToInt d) 0

-- | Try @p@, return @x@ if @p@ fails w/o consuming input.
option' :: Alternative f => a -> f a -> f a
option' x p = p <|> pure x
