module Regression.LexerCombinators.Generated where

import Data.Char (isAlpha, isAlphaNum, isSpace)

import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer qualified as L
import Text.Gigaparsec (Parsec, many, eof)

import Regression.LexerCombinators.Shared 

-------------------------------------------------------------------------------
-- Parsers

manyIdents :: Parsec [String]
manyIdents = many identifier <* eof


-------------------------------------------------------------------------------
-- Lexing setup

lexicalDesc :: D.LexicalDesc
lexicalDesc = D.plain {
    D.nameDesc = nameDesc
  , D.spaceDesc = spaceDesc
  }

nameDesc :: D.NameDesc
nameDesc = D.plainName {
    D.identifierStart  = Just identStart
  , D.identifierLetter = Just identLetter
  }

spaceDesc :: D.SpaceDesc
spaceDesc = D.plainSpace {
    D.space = Just isSpace
  , D.whitespaceIsContextDependent = False
  , D.lineCommentStart = lineCommentStart
  , D.multiLineCommentStart = multiLineCommentStart
  , D.multiLineCommentEnd = multiLineCommentEnd
  , D.multiLineNestedComments = False
  , D.lineCommentAllowsEOF = True
  }

lexer :: L.Lexer
lexer = L.mkLexer lexicalDesc

spaces :: Parsec ()
spaces = L.whiteSpace (L.space lexer)

identifier :: Parsec String
identifier = L.identifier (L.names (L.lexeme lexer))
