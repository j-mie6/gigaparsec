{-# LANGUAGE
      TemplateHaskell
  #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TupleSections #-}
module JavascriptBench.FlatParse.TH where

import Data.ByteString qualified as B hiding (unpack)
import Data.ByteString.Char8 qualified as B

import FlatParse.Basic qualified as FP
import FlatParse.Basic hiding (Parser)

import Shared.FlatParse.Extended

import Language.Haskell.TH (Code (examineCode, Code), Q, unsafeCodeCoerce, unTypeCode, Exp (LamCaseE, CaseE, UnboundVarE), bindCode, mkName, liftCode)
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

opLetter :: Parser Char
opLetter = undefined

prefixOp :: Code Q (Parser (JSUnary -> JSUnary))
prefixOp = switchTyped [||\case
  "--" -> pure JSDec
  "++" -> pure JSInc
  "-"  -> pure JSNeg
  "+"  -> pure JSPlus
  "~"  -> pure JSBitNeg
  "!"  -> pure JSNot
  ||]

postfixOp :: Code Q (Parser (JSUnary -> JSUnary))
postfixOp = switchTyped [||\case
  "--" -> pure JSDec
  "++" -> pure JSInc
  ||]


switchTyped :: Code Q (String -> Parser a) -> Code Q (Parser a)
switchTyped code = 
  bindCode (unTypeCode code) $ \e -> 
    case e of
      LamCaseE cases -> 
        let exp = CaseE (UnboundVarE (mkName "_")) cases
        in  unsafeCodeCoerce (switch (pure exp))
      _ -> liftCode (fail "FlatParse.Utils.switchTyped: expected a `\\case` expression.")

binOp :: Code Q (Parser (Int, JSExpr' -> JSExpr' -> JSExpr'))
binOp = switchTyped [||\case
  "*"  -> pure (9, JSMul)
  "/"  -> pure (9, JSDiv)
  "%"  -> pure (9, JSMod)
  "+"  -> pure (8, JSAdd)
  "-"  -> pure (8, JSSub)
  "<<" -> pure (7, JSShl)
  ">>" -> pure (7, JSShr)
  "<=" -> pure (6, JSLe)
  "<"  -> pure (6, JSLt)
  ">=" -> pure (6, JSGe)
  ">"  -> pure (6, JSGt)
  "==" -> pure (5, JSEq)
  "!=" -> pure (5, JSNe)
  "|"  -> pure (4, JSBitOr)
  "^"  -> pure (3, JSBitXor)
  "&"  -> pure (2, JSBitAnd)
  "&&" -> pure (1, JSAnd)
  "||" -> pure (0, JSOr)
  ||]

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
