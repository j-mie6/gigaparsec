{-# LANGUAGE 
      OverloadedLists
    , TemplateHaskell
#-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JavascriptBench.Gigaparsec.Configured.Lexer where

import Data.Char (readLitChar, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)

import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer (Lexer)
import Text.Gigaparsec.Token.Lexer qualified as L

import JavascriptBench.Shared (jsIdentStart, jsIdentLetter, jsKeywords)
import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Internal.TH.VersionAgnostic (Extension(TemplateHaskell))
import Text.Gigaparsec.Token.Patterns (lexerCombinators, lexerCombinatorsWithNames, overloadedStrings)




lexer :: L.Lexer
lexer = L.mkLexer lexicalDesc


lexicalDesc :: D.LexicalDesc
lexicalDesc = D.plain {
    D.nameDesc = nameDesc
  , D.symbolDesc = symbolDesc
  , D.numericDesc = numericDesc
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
-- Numeric Description

numericDesc :: D.NumericDesc
numericDesc = D.plainNumeric

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
  ])

$(overloadedStrings [| lexer |])

string :: Parsec String
string = L.ascii stringLiteral
    <|>  L.ascii multiStringLiteral
