{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Haskell.Lexer where

import Haskell.Lexer.Description

import Text.Gigaparsec.Token.Lexer qualified as Lexer
import Text.Gigaparsec.Token.Lexer (Lexer, mkLexer)
import Text.Gigaparsec.Token.Patterns (overloadedStrings)
import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Data.Char (isLower, isAlphaNum, isSymbol)
import Data.Set qualified as Set
import Data.Set (Set)
import Text.Gigaparsec.Char
import Text.Gigaparsec (Parsec, many, (<|>))

lexer :: Lexer
lexer = mkLexer desc

$(overloadedStrings [|lexer|])

reservedOperator :: String -> Parsec ()
reservedOperator = Lexer.softOperator (Lexer.symbol hLexeme)

lexeme :: Parsec a -> Parsec a
lexeme = Lexer.apply hLexeme

keyword :: String -> Parsec ()
keyword = Lexer.softKeyword (Lexer.symbol hLexeme)

operator :: Parsec String
operator = Lexer.userDefinedOperator hNames

hLexeme :: Lexer.Lexeme
hLexeme = Lexer.lexeme lexer

hNames :: Lexer.Names
hNames = Lexer.names hLexeme

variable :: Parsec String
variable = Lexer.identifier hNames

constructor :: Parsec String
constructor = lexeme ((:) <$> upper <*> many letterOrDigit)

constructorOp :: Parsec String
constructorOp = Lexer.userDefinedOperator' hNames (Just (== ':'))

integer :: Parsec Integer
integer = 
      wrap Lexer.decimal
  <|> wrap Lexer.hexadecimal
  <|> wrap Lexer.binary
  where
    wrap p = p (Lexer.integer hLexeme)

fully = Lexer.fully lexer

lambda :: Parsec ()
lambda = "λ" <|> "\\"

arrow :: Parsec ()
arrow = "->" <|> "→"
