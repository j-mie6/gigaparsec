{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies,
                -Wno-orphans,
                -Wmissing-kind-signatures
#-}
{-|
A small lang containing expressions and assignment.
One statement per newline, except within parens, in which newlines are not significant.
This is useful for testing the internal whitespace parser implementations.

ExprLang programs consist of a series of assignments and a final return statement.
Each assignment must be on a newline; if an expression is written over multiple lines, it must be within parentheses.

An example program would be,

> x = 1 + 2
> y = (1 + 
  2 + 3)
> z = (
    x +
    2
  )
> return y + z * 3

Implemented using the patterns found in:
Willis & Wu - Design patterns for parser combinators (functional pearl)
https://dl.acm.org/doi/10.1145/3471874.3472984
-}
module Main where

import           ExprLang.Parser (program, expr)
import ExprLang.AST
import ExprLang.Lexer (fully)

import           Text.Gigaparsec (Parsec, Result (..), parseFromFile, parse)
import System.IO.Error (userError)
import Control.Exception (throwIO)

main :: IO ()
main = do
  parseFilePretty =<< readLn


parseFile :: FilePath -> IO Program
parseFile f = do
  x <- parseFromFile @String program f
  case x of
    Success p -> return p
    Failure e -> throwIO (userError e)

parseFileShow :: FilePath -> IO ()
parseFileShow x = print =<< parseFile x

parseFilePretty :: FilePath -> IO ()
parseFilePretty x = putStrLn . pretty =<< parseFile x


parseString :: Parsec a -> String -> Result String a
parseString p = parse (fully p)

parsePretty :: Pretty a => Parsec a -> String -> String
parsePretty p x = case parseString p x of
  Success y -> pretty y
  Failure e -> "Error: " ++ e

parseProg :: String -> Result String Program
parseProg = parseString program

parseExpr :: String -> Result String Expr
parseExpr = parseString expr

parsePrettyExpr :: String -> IO ()
parsePrettyExpr = putStrLn . parsePretty expr

parsePrettyProg :: String -> IO ()
parsePrettyProg = putStrLn . parsePretty program
