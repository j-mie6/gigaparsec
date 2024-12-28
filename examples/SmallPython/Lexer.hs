{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

{-|
This module defines the lexer for (small)python, and exposes the core combinators for processing tokens.

It is defined in terms of the lexical description in "SmallPython.Lexer.Description".
-}
module SmallPython.Lexer (
  keyword, kw,
  operator,
  -- binop, unop,
  integer,
  ident,
  alter,
  isNormalWhiteSpace,
  isParenWhiteSpace,
  whiteSpace,
  fully,
  nextLine,
  indent1,
  indent1After,
  thenIndent1,
  indent1At,
  nonIndented,
  ) where

import Prelude hiding (fail)

import SmallPython.Lexer.Description (pythonDesc, isKeyword, isReservedOperator, reservedBinOps, isNormalWhiteSpace, isParenWhiteSpace)

import Text.Gigaparsec

import Text.Gigaparsec.Token.Lexer qualified as Lexer
import Text.Gigaparsec.Token.Indentation qualified as Lexer
import Text.Gigaparsec.Token.Descriptions qualified as Desc

import Text.Gigaparsec.Errors.Combinator (fail)

import GHC.Unicode (isAlpha, isAlphaNum, isSpace, isPrint)
import Text.Gigaparsec.Char (string, newline, endOfLine)
import GHC.Exts (IsString(fromString))
import SmallPython.AST (BinOp, binOpMap, UnaryOp, unaryOpMap, mkBinOp, mkUnaryOp, ParseInfo (..), parseInfo)

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty (NonEmpty)

instance a ~ () => IsString (Parsec a) where
  fromString :: String -> Parsec a
  fromString x 
    | isKeyword x = keyword x
    | isReservedOperator x = operator x
    | otherwise = string x *> unit

{-# inline pythonLexer #-}
pythonLexer :: Lexer.Lexer
pythonLexer = Lexer.mkLexer (pythonDesc)

{-# inline pythonLexeme #-}
pythonLexeme :: Lexer.Lexeme
pythonLexeme = Lexer.lexeme pythonLexer

{-# inline keyword #-}
keyword :: String -> Parsec ()
keyword = Lexer.softKeyword (Lexer.symbol pythonLexeme)

{-# inline kw #-}
kw :: String -> Parsec ()
kw = keyword

{-# inline operator #-}
operator :: String -> Parsec ()
operator = Lexer.softOperator (Lexer.symbol pythonLexeme)

-- -- These should probably be done in the parser bridge.
-- binop :: String -> Parsec BinOp
-- binop x = do
--   case (Map.lookup x binOpMap) of
--     Just binOp -> operator x *> mkBinOp binOp
--     Nothing -> fail [("Internal: bad binop: " ++ x)]

-- unop :: String -> Parsec UnaryOp
-- unop x = do
--   p <- parseInfo
--   case (Map.lookup x unaryOpMap) of
--     Just op -> operator x *> return (mkUnaryOp p op)
--     Nothing -> fail [("Internal: bad unaryop: " ++ x)]

---------------------------------------------------------------------------------------------------
-- Names

{-# inline pythonNames #-}
pythonNames :: Lexer.Names
pythonNames = Lexer.names pythonLexeme

{-# inline ident #-}
ident :: Parsec String
ident = Lexer.identifier pythonNames


---------------------------------------------------------------------------------------------------
-- Integers

-- Python doesn't have unsigned ints
pythonIntegers :: Lexer.IntegerParsers Lexer.CanHoldSigned
pythonIntegers = Lexer.integer pythonLexeme

-- Python ints are unbounded
{-# inline integer #-}
integer :: Parsec Integer
integer = 
      Lexer.decimal pythonIntegers
  <|> Lexer.hexadecimal pythonIntegers
  <|> Lexer.octal pythonIntegers
  <|> Lexer.binary pythonIntegers

---------------------------------------------------------------------------------------------------
-- Whitespace


{-# inline alter #-}
alter :: (Char -> Bool) -> Parsec a -> Parsec a
alter = Lexer.alter pythonSpace . Just

{-# inline pythonSpace #-}
pythonSpace :: Lexer.Space
pythonSpace = Lexer.space pythonLexer

{-# inline whiteSpace #-}
whiteSpace :: Parsec ()
whiteSpace = Lexer.whiteSpace pythonSpace

{-# inline fully #-}
fully :: Parsec a -> Parsec a
fully = Lexer.fully pythonLexer

{-# inline nextLine #-}
nextLine :: Parsec ()
nextLine = Lexer.apply pythonLexeme (void endOfLine)

{-# inline indent1 #-}
indent1 :: Parsec b -> Parsec (NonEmpty b)
indent1 = Lexer.indent1 pythonSpace

{-# inline indent1After #-}
indent1After :: Parsec a -> Parsec b -> Parsec (a, NonEmpty b)
indent1After = Lexer.indentAfter1 pythonSpace

{-# inline indent1At #-}
indent1At :: Lexer.IndentLevel -> Parsec b -> Parsec (NonEmpty b)
indent1At = (`Lexer.indentSomeAt` pythonSpace)

{-# inline thenIndent1 #-}
thenIndent1 :: Parsec a -> Parsec b -> Parsec (a, NonEmpty b)
thenIndent1 = Lexer.thenIndent1 pythonSpace


{-# inline nonIndented #-}
nonIndented :: Parsec a -> Parsec a
nonIndented = Lexer.nonIndented pythonSpace
