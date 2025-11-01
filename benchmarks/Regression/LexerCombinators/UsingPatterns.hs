{-# LANGUAGE 
      TemplateHaskell
  #-}
module Regression.LexerCombinators.UsingPatterns where

import Data.Char (isAlpha, isAlphaNum, isSpace)

import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer qualified as L
import Text.Gigaparsec.Token.Patterns (lexerCombinators)
import Text.Gigaparsec (Parsec, many, eof)

import Regression.LexerCombinators.Shared 

import Regression.LexerCombinators.Generated (lexer)

-------------------------------------------------------------------------------
-- Parsers

$(lexerCombinators [| lexer |] [
    'L.identifier
  ])

manyIdents :: Parsec [String]
manyIdents = many identifier <* eof

