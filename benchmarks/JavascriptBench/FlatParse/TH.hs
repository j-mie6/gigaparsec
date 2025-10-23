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

opLetter :: Parser Char
opLetter = undefined

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
postfixOp = undefined
-- switchTyped [|| case "" of
--   "--" -> jsDec
--   "++" -> jsInc
  -- ||]



expr' :: Code Q (Parser JSExpr')
expr' = [|| 
      $$prefixOp
  <|> $$infixOp
  ||]

switchTyped :: Code Q (Parser a) -> Code Q (Parser a)
switchTyped code = unsafeCodeCoerce (switch (unTypeCode code))

jsAtom :: Parser JSUnary
jsAtom = undefined

infixOp :: Code Q (Parser JSExpr')
infixOp = 
  let 
    
    foo :: Code Q (Parser JSExpr')
    foo = [|| 
      let go :: Int -> (JSExpr' -> JSExpr') -> Parser JSExpr' = \(prevPrec :: Int) (k :: (JSExpr' -> JSExpr')) -> do
            (exprₗ :: JSExpr') <- JSUnary <$> jsAtom
            (op :: Maybe ((Int, JSExpr' -> JSExpr'))) <- 
              optional ($$op2 <*> pure prevPrec <*> pure k <*> pure exprₗ)
            case op of
              Nothing -> pure (k exprₗ)
              Just (newPrec, k') -> go newPrec k'
      in go 9 id
      ||]
  in foo -- [|| ((JSUnary <$> $$base) <**> $$op2) <*> pure id ||]
  where
  op2 = switchTyped [|| case "" of 
      "*"  -> pure (infix3 9 JSMul)
      "/"  -> pure (infix3 9 JSDiv)
      "%"  -> pure (infix3 9 JSMod)
      "+"  -> pure (infix3 8 JSAdd)
      "-"  -> pure (infix3 8 JSSub)
      "<<" -> pure (infix3 7 JSShl)
      ">>" -> pure (infix3 7 JSShr)
      "<=" -> pure (infix3 6 JSLe)
      "<"  -> pure (infix3 6 JSLt)
      ">=" -> pure (infix3 6 JSGe)
      ">"  -> pure (infix3 6 JSGt)
      "==" -> pure (infix3 5 JSEq)
      "!=" -> pure (infix3 5 JSNe)
      "|"  -> pure (infix3 4 JSBitOr)
      "^"  -> pure (infix3 3 JSBitXor)
      "&"  -> pure (infix3 2 JSBitAnd)
      "&&" -> pure (infix3 1 JSAnd)
      "||" -> pure (infix3 0 JSOr)
      -- _    -> base
      ||]

  infix2 
    :: Int 
    -> (JSExpr' -> JSExpr' -> JSExpr')
    -> Code Q (
           Int 
        -> (JSExpr' -> JSExpr') 
        -> JSExpr' 
        -> (Int, JSExpr' -> JSExpr')
      )
  infix2 inner f = [|| \outer  ->
      if outer >= inner 
        then \k x -> (inner, f (k x))
        else \k x -> (inner, k . (f x))
    ||]

  infix3 
    :: Int 
    -> (JSExpr' -> JSExpr' -> JSExpr')
    -> Int 
    -> (JSExpr' -> JSExpr') 
    -> JSExpr' 
    -> (Int, JSExpr' -> JSExpr')
  infix3 curPrec f prevPrec k x = 
    (curPrec,) $
      if prevPrec >= curPrec 
        then f (k x)
        else k . (f x)


  
--   postfix 
--     :: Code Q (JSExpr' -> JSExpr') 
--     -> Code Q (Parser (JSExpr' -> (JSExpr' -> JSExpr') -> JSExpr'))
--   postfix f = [|| pure \x k -> k ($$f x)  ||]

--   postfix2 
--     :: (JSExpr' -> JSExpr')
--     -> Parser (JSExpr' -> (JSExpr' -> JSExpr') -> JSExpr')
--   postfix2 f = pure \x k -> k (f x)
--   infixE 
--     :: Int 
--     -> Code Q (JSExpr' -> JSExpr' -> JSExpr') 
--     -> Code Q (Int -> Parser (JSExpr' -> (JSExpr' -> JSExpr') -> JSExpr'))
--   infixE inner f = [|| \outer -> 
--     let foo = (\x k -> $$f (k x))
--     in
--     if outer >= inner
--       then pure (\x k -> $$f (k x))
--       else pure (\x k y -> k ($$f x) y)
--     ||]

-- infixBinExpr :: Int -> Code Q (JSExpr' -> JSExpr' -> JSExpr') -> Q Exp
-- infixBinExpr inner f = unTypeCode go
--   where
--   go :: Code Q (Int -> Parser ((JSExpr' -> JSExpr') -> JSExpr' -> JSExpr' -> JSExpr'))
--   go = [|| \outer -> 
--     pure if outer >= inner
--       then (\k x y -> $$f (k x) y)
--       else (\k x y -> k ($$f x y))
--     ||]

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
