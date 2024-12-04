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
  nextLine
  ) where

import Prelude hiding (fail)

import SmallPython.Lexer.Description (pythonDesc, isKeyword, isReservedOperator, reservedBinOps, isNormalWhiteSpace, isParenWhiteSpace)

import Text.Gigaparsec

import Text.Gigaparsec.Token.Lexer qualified as Lexer
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


alter :: (Char -> Bool) -> Parsec a -> Parsec a
alter = Lexer.alter pythonSpace . Just

pythonSpace :: Lexer.Space
pythonSpace = Lexer.space pythonLexer

whiteSpace :: Parsec ()
whiteSpace = Lexer.whiteSpace pythonSpace


fully :: Parsec a -> Parsec a
fully = Lexer.fully pythonLexer

nextLine :: Parsec ()
nextLine = Lexer.apply pythonLexeme (void endOfLine)
