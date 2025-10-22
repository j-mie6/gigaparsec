{-# LANGUAGE
      TemplateHaskell
  #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module JavascriptBench.FlatParse.TH where

import Data.ByteString qualified as B hiding (unpack)
import Data.ByteString.Char8 qualified as B

import FlatParse.Basic qualified as FP
import FlatParse.Basic hiding (Parser)

import Shared.FlatParse.Utils 

import Language.Haskell.TH (Code, Q, unsafeCodeCoerce)
import JavascriptBench.Shared

tokenChar :: Char -> Code Q (Parser ())
tokenChar c = unsafeCodeCoerce (FP.char c)

token :: String -> Code Q (Parser String)
token xs = [|| $$(unsafeCodeCoerce (FP.string xs)) *> pure xs ||]

skipToken :: String -> Code Q (Parser ())
skipToken xs = unsafeCodeCoerce (FP.string xs)

symbol :: Char -> Code Q (Parser ())
symbol c = [|| $$(unsafeCodeCoerce (FP.char c)) <* whitespace ||]


oneOf2 :: Char -> Char -> Code Q (Parser Char)
oneOf2 c1 c2 = [|| ($$(tokenChar c1) *> pure c1) <|> ($$(tokenChar c2) *> pure c2) ||]

keyword :: String -> Code Q (Parser ())
keyword s = [|| notFollowedBy $$(unsafeCodeCoerce (string s)) identLetter *> whitespace ||]


identStart :: Parser Char
identStart = satisfy jsIdentStart

identLetter :: Parser Char
identLetter = satisfy jsIdentLetter


-------------------------------------------------------------------------------
-- Whitespace

space :: Parser ()
space = skipSatisfyAscii (\c -> c == ' ' || c == '\n' || c == '\t' || c == '\r')

lineCommentSuffix :: Parser ()
lineCommentSuffix = 
  withOption anyWord8
    (\case 10 -> whitespace
           _  -> lineCommentSuffix)
    (pure ())

multilineCommentSuffix :: Parser ()
multilineCommentSuffix = go (1 :: Int) where
  go 0 = whitespace
  go n = $(switch [| case _ of
    "*/" -> go (n - 1)
    "/*" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

whitespace :: Parser ()
whitespace = $(switch [| case _ of 
    " "  -> whitespace
    "\n" -> whitespace
    "\t" -> whitespace
    "\r" -> whitespace
    "//" -> lineCommentSuffix
    "/*" -> multilineCommentSuffix
  |])

spaces :: Parser ()
spaces = skipSome space
