{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies,
                -Wno-orphans,
                -Wmissing-kind-signatures
#-}
{-|
A small lang containing expressions and assignment.
One statement per newline, except within parens, in which newlines are not significant.
This is useful for testing the internal whitespace parser implementations.

Implemented using the evergreen patterns found in:
Willis & Wu - Design patterns for parser combinators (functional pearl)
https://dl.acm.org/doi/10.1145/3471874.3472984
-}
module ExprLang where

import           ExprLang.Parser (program, expr)
import ExprLang.AST

import           Text.Gigaparsec (Parsec, Result (..), parseFromFile, parse)
import System.IO.Error (userError)
import Control.Exception (throwIO)


parseFile :: FilePath -> IO Program
parseFile f = do
  x <- parseFromFile @String program f
  case x of
    Success p -> return p
    Failure e -> throwIO (userError e)

parseShow :: FilePath -> IO ()
parseShow x = print =<< parseFile x

parsePretty :: FilePath -> IO ()
parsePretty x = putStrLn . pretty =<< parseFile x

_parsePString :: Pretty a => Parsec a -> String -> String
_parsePString p x = case parse p x of
  Success y -> pretty y
  Failure e -> "Error: " ++ e


parseProg :: String -> String
parseProg = _parsePString program

parseExpr :: String -> String
parseExpr = _parsePString expr

