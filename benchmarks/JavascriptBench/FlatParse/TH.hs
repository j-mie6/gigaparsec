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

import Language.Haskell.TH (Code, Q, unsafeCodeCoerce, unTypeCode, Exp)
import JavascriptBench.Shared
import Data.Set qualified as Set
import Control.Applicative ((<**>), liftA3, liftA)

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

operator :: String -> Code Q (Parser ())
operator s = [|| notFollowedBy $$(unsafeCodeCoerce (string s)) opLetter *> whitespace ||]

identStart :: Parser Char
identStart = satisfy jsIdentStart

identLetter :: Parser Char
identLetter = satisfy jsIdentLetter

expr9 :: Code Q (Parser JSExpr')
expr9 = [|| chainl1 $$expr8 $$op ||]
  where
  op :: Code Q (Parser (JSExpr' -> JSExpr' -> JSExpr'))
  op = switchTyped [|| case "" of 
      "*"  -> pure JSMul
      "/"  -> pure JSDiv
      "%"  -> pure JSMod
      ||]

expr8 :: Code Q (Parser JSExpr')
expr8 = _

prefixOp :: Code Q (Parser JSExpr')
prefixOp = switchTyped [|| case "" of
  "--" -> jsDec <$> $$expr'
  "++" -> jsInc <$> $$expr'
  "-"  -> jsNeg <$> $$expr'
  "+"  -> jsPlus <$> $$expr'
  "~"  -> jsBitNeg <$> $$expr'
  "!"  -> jsNot <$> $$expr'
  ||]

postfixOp :: Code Q (Parser JSExpr')
postfixOp = switchTyped [|| case "" of
  "--" -> jsDec
  "++" -> jsInc
  ||]

expr' :: Code Q (Parser JSExpr')
expr' = [|| 
      $$prefixOp
  <|> $$(infixOp 9)
  ||]

switchTyped :: Code Q (Parser a) -> Code Q (Parser a)
switchTyped code = unsafeCodeCoerce (switch (unTypeCode code))

infixOp :: Int -> Code Q (Parser JSUnary) -> Code Q (Parser JSExpr')
infixOp outer base = 
  let 
    op2 = switchTyped [|| case "" of 
      -- catch postfix cases that clash with binops
      "++" -> $$(infixBinExpr ) 
      "--" -> _

      "*"  -> $$(infixBinExprTyped 9 base [||JSMul||]) outer
      "/"  -> $$(infixBinExprTyped 9 base [||JSDiv||]) outer
      "%"  -> $$(infixBinExprTyped 9 base [||JSMod||]) outer
      "+"  -> $$(infixBinExprTyped 8 base [||JSAdd||]) outer
      "-"  -> $$(infixBinExprTyped 8 base [||JSSub||]) outer
      "<<" -> $$(infixBinExprTyped 7 base [||JSShl||]) outer
      ">>" -> $$(infixBinExprTyped 7 base [||JSShr||]) outer
      "<=" -> $$(infixBinExprTyped 6 base [||JSLe||]) outer
      "<"  -> $$(infixBinExprTyped 6 base [||JSLt||]) outer
      ">=" -> $$(infixBinExprTyped 6 base [||JSGe||]) outer
      ">"  -> $$(infixBinExprTyped 6 base [||JSGt||]) outer
      "==" -> $$(infixBinExprTyped 5 base [||JSEq||]) outer
      "!=" -> $$(infixBinExprTyped 5 base [||JSNe||]) outer
      "|"  -> $$(infixBinExprTyped 4 base [||JSBitOr||]) outer
      "^"  -> $$(infixBinExprTyped 3 base [||JSBitXor||]) outer
      "&"  -> $$(infixBinExprTyped 2 base [||JSBitAnd||]) outer
      "&&" -> $$(infixBinExprTyped 1 base [||JSAnd||]) outer
      "||" -> $$(infixBinExprTyped 0 base [||JSOr||]) outer
      _    -> base
      ||]
  in [|| ((JSUnary <$> $$base) <**> $$op2) <*> pure id ||]
  where
  postfix 
    :: Code Q (JSExpr' -> JSExpr') 
    -> Code Q (Parser (JSExpr' -> (JSExpr' -> JSExpr') -> JSExpr'))
  postfix f = [|| _ ||]

  infixBinExprTyped 
    :: Int 
    -> Code Q (Parser JSUnary)
    -> Code Q (JSExpr' -> JSExpr' -> JSExpr') 
    -> Code Q (Int -> Parser (JSExpr' -> (JSExpr' -> JSExpr') -> JSExpr'))
  infixBinExprTyped inner base f = [|| \outer -> 
    if outer >= inner
      then (\y x k -> $$f (k x) y) <$> $$(infixOp inner base)
      else (\y x k -> k ($$f x y)) <$> $$(infixOp inner base)
    ||]

infixBinExpr :: Int -> Code Q (JSExpr' -> JSExpr' -> JSExpr') -> Q Exp
infixBinExpr inner f = unTypeCode go
  where
  go :: Code Q (Int -> Parser ((JSExpr' -> JSExpr') -> JSExpr' -> JSExpr' -> JSExpr'))
  go = [|| \outer -> 
    pure if outer >= inner
      then (\k x y -> $$f (k x) y)
      else (\k x y -> k ($$f x y))
    ||]

-- opLetter :: Parser Char
-- opLetter = switchFromSet (Set.fromList $ map (: []) "+-*/=<>!~&|.%^") _


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
