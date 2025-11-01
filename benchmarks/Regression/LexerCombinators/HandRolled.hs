module Regression.LexerCombinators.HandRolled where

import Data.Char (isAlpha, isAlphaNum, isSpace)

import Text.Gigaparsec
import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer qualified as L

import Regression.LexerCombinators.Shared 
import Text.Gigaparsec.Char hiding (whitespace, spaces, space)
import Text.Gigaparsec.Combinator
import Data.Set qualified as Set


-------------------------------------------------------------------------------
-- Parsers

manyIdents :: Parsec [String]
manyIdents = many identifier <* eof

-------------------------------------------------------------------------------
-- Lexing setup

-- | A token is (a possibly multi-char) something which is either totally parsed
-- or not at all, the latter not consuming input.
token :: String -> Parsec String
token = atomic . string


-- Token Parsers
space :: Parsec ()
space = void (satisfy isSpace)

spaces :: Parsec ()
spaces = skipSome space

whitespace :: Parsec ()
whitespace = skipMany (spaces <|> oneLineComment <|> multiLineComment)

oneLineComment :: Parsec ()
oneLineComment = void (token "//" *> skipMany (satisfy (/= '\n')))

multiLineComment :: Parsec ()
multiLineComment = 
  let inComment = void (token "*/")
              <|> skipSome (noneOf (Set.fromList "/*")) *> inComment
              <|> oneOf (Set.fromList "/*") *> inComment
  in token "/*" *> inComment

identifier :: Parsec String
identifier = atomic (filterS unreservedName (satisfy identStart <:> many (satisfy identLetter))) <* whitespace
