{-# LANGUAGE OverloadedLists #-}
module Haskell.Lexer.Description where
import Text.Gigaparsec.Token.Lexer qualified as Lexer
import Text.Gigaparsec.Token.Lexer (Lexer)
import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Data.Char (isLower, isAlphaNum, isSymbol)
import Data.Set qualified as Set
import Data.Set (Set)
import Text.Gigaparsec.Char
import Text.Gigaparsec (Parsec, many)

desc :: Desc.LexicalDesc
desc = Desc.plain {
    Desc.nameDesc    = nameDesc,
    Desc.symbolDesc  = symbolDesc,
    Desc.numericDesc = numericDesc,
    Desc.textDesc    = textDesc,
    Desc.spaceDesc   = spaceDesc
  }
  where
    nameDesc = Desc.plainName {
        Desc.identifierStart = Just isLower,
        Desc.identifierLetter = Just isAlphaNum,
        Desc.operatorStart = Just isOperatorStart,
        Desc.operatorLetter = Just isOperatorLetter
      }

    symbolDesc = Desc.plainSymbol {
        Desc.hardKeywords = hardKeywords,
        Desc.hardOperators = hardOperators
      }
    numericDesc = Desc.plainNumeric
    textDesc = Desc.plainText
    spaceDesc = Desc.plainSpace

hardKeywords :: Set String
hardKeywords = [
  "if", "then", "else", "data", "where",
  "let", "in", "case", "of", "λ"
  ]

hardOperators :: Set String
hardOperators = [
  "$", "||", "&&", "<", "<=", ">", ">=", "==", "/=", ":",
  "++", "+", "-", "*", "/", "^", ".", "\\", "→", "->"
  ]


specialSymbols :: Set Char
specialSymbols = [
    '(',
    ')',
    ',',
    ';',
    '[',
    ']',
    '`',
    '{',
    '}',
    '⦃',
    '⦄',
    '⟦',
    '⟧'
  ]

disallowedSymbols :: Set Char
disallowedSymbols = [
    '_',
    ':',
    '"',
    '\''
  ]


isOperatorStart :: Char -> Bool
isOperatorStart x = 
      isSymbol x 
  &&  Set.notMember x specialSymbols
  &&  Set.notMember x disallowedSymbols


isOperatorLetter :: Char -> Bool
isOperatorLetter x = x == ':' || isOperatorStart x
