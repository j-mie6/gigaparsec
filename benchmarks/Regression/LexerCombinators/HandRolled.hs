module Regression.LexerCombinators.HandRolled where

import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper)

import Text.Gigaparsec
import Text.Gigaparsec.Token.Descriptions qualified as D
import Text.Gigaparsec.Token.Lexer qualified as L

import Regression.LexerCombinators.Shared 
import Text.Gigaparsec.Char hiding (whitespace, spaces, space)
import Text.Gigaparsec.Combinator
import Data.Set qualified as Set
import Data.Set (Set)
import Control.Applicative (Alternative)

-- | Try @p@, return @x@ if @p@ fails w/o consuming input.
option' :: Alternative f => a -> f a -> f a
option' x p = p <|> pure x

-------------------------------------------------------------------------------
-- Parsers

manyIdents :: Parsec [String]
manyIdents = many (stringLit <|> identifier) <* eof

-------------------------------------------------------------------------------
-- Lexing setup

-- | A token is (a possibly multi-char) something which is either totally parsed
-- or not at all, the latter not consuming input.
token :: String -> Parsec String
token = atomic . string

-------------------------------------------------------------------------------
-- Whitespace 

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

-------------------------------------------------------------------------------
-- Idents

identifier :: Parsec String
identifier = atomic (filterS unreservedName (satisfy identStart <:> many (satisfy identLetter))) <* whitespace

-------------------------------------------------------------------------------
-- Strings

-- The set of possible characters starting an escape character sequence.
escChrs :: Set Char
escChrs = Set.fromList "abfntv\\\"'0123456789xo^ABCDEFGHLNRSUV"

stringLit :: Parsec [Char]
stringLit = token "\"" *> many stringChar <* token "\"" <* whitespace

-- | Parse a single character in a js string
stringChar :: Parsec Char
stringChar = satisfy stringLetter <|> stringEscape

-- | Parse a single escape character sequence in a js string
stringEscape :: Parsec Char
stringEscape = token "\\" *> escapeCode

-- Parses any permissible escape code that can appear after a '\'
escapeCode :: Parsec Char
escapeCode = oneOf escChrs >>= escCode
-- match escChrs (oneOf escChrs) escCode empty
  where
    -- Given the starting character, how to parse the rest of the escape sequence.
    escCode :: Char -> Parsec Char
    escCode 'a' = pure ('\a')
    escCode 'b' = pure ('\b')
    escCode 'f' = pure ('\f')
    escCode 'n' = pure ('\n')
    escCode 't' = pure ('\t')
    escCode 'v' = pure ('\v')
    escCode '\\' = pure ('\\')
    escCode '"' = pure ('"')
    escCode '\'' = pure ('\'')
    escCode '^' = (\c -> toEnum (fromEnum c - fromEnum 'A' + 1)) <$> satisfy isUpper
    escCode 'A' = token "CK" $> ('\ACK')
    escCode 'B' = token "S" $> ('\BS') <|> token "EL" $> ('\BEL')
    escCode 'C' = token "R" $> ('\CR') <|> token "AN" $> ('\CAN')
    escCode 'D' = token "C" *> (token "1" $> ('\DC1')
                          <|> token "2" $> ('\DC2')
                          <|> token "3" $> ('\DC3')
                          <|> token "4" $> ('\DC4'))
            <|> token "EL" $> ('\DEL')
            <|> token "LE" $> ('\DLE')
    escCode 'E' = token "M" $> ('\EM')
            <|> token "T" *> (token "X" $> ('\ETX')
                          <|> token "B" $> ('\ETB'))
            <|> token "SC" $> ('\ESC')
            <|> token "OT" $> ('\EOT')
            <|> token "NQ" $> ('\ENQ')
    escCode 'F' = token "F" $> ('\FF') <|> token "S" $> ('\FS')
    escCode 'G' = token "S" $> ('\GS')
    escCode 'H' = token "T" $> ('\HT')
    escCode 'L' = token "F" $> ('\LF')
    escCode 'N' = token "UL" $> ('\NUL') <|> token "AK" $> ('\NAK')
    escCode 'R' = token "S" $> ('\RS')
    escCode 'S' = token "O" *> option' (('\SO')) (token "H" $> ('\SOH'))
            <|> token "I" $> ('\SI')
            <|> token "P" $> ('\SP')
            <|> token "TX" $> ('\STX')
            <|> token "YN" $> ('\SYN')
            <|> token "UB" $> ('\SUB')
    escCode 'U' = token "S" $> ('\US')
    escCode 'V' = token "T" $> ('\VT')
    -- TODO numeric
    escCode _ = empty--error "numeric escape codes not supported"
