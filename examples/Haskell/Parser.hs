{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Haskell.Parser where

import Haskell.Lexer
import Haskell.AST

import Data.List.NonEmpty (NonEmpty)

import Text.Gigaparsec hiding (some)
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Errors.Combinator ((<?>))
import Text.Gigaparsec.Expr ((+<), (>+), sops, precedence, Fixity(InfixL), Prec)
import Text.Gigaparsec.Expr qualified as Expr (Prec(..))
import Text.Gigaparsec.Position (pos)
import Text.Gigaparsec.Combinator.NonEmpty (some)

atom :: Parsec Atom
atom = 
      AtomInt <$> integer
  <|> AtomVar <$> variable
  <|> AtomCstr <$> constructor


mExprBin :: Parsec (BinOp -> Expr -> Expr -> Expr)
mExprBin = do
  p <- pos
  return (\op t u -> ExprBin p op t u)

binOp :: Parsec BinOp
binOp = BinOp <$> operator



pattern_ :: Parsec Pattern
pattern_ = mkPatVar variable

patterns :: Parsec (NonEmpty Pattern)
patterns = some pattern_
  
paren :: Parsec a -> Parsec a
paren p = "(" *> p <* ")"

clause :: Parsec Clause
clause = mkClause variable (many pattern_) ("=" *> expr)

term :: Parsec Expr
term = 
      mkExprAtom <*> atom
  <|> mkExprTuple ("(" *> sepBy expr "," <* ")")
  <|> mkExprParen (paren expr)

funApp :: Parsec Expr
funApp = do
  t <- term
  (mkExprApp <*> pure t <*> some expr)
    <|> return t


expr :: Parsec Expr
expr = precedence $ 
      sops InfixL [mkExprBin <*> ("+" $> BinOp "+")]
  +<  sops InfixL [mkExprBin <*> binOp]
  +<  expr10 
  -- Expr.Atom ((mkExprAtom <*> atom) <|> paren expr <|> funApp)

caseClauses :: Parsec [(Pattern, Expr)]
caseClauses = undefined

expr10 :: Prec Expr
expr10 = Expr.Atom (
      (mkExprLam (lambda *> patterns) (arrow *> expr) <?> ["lambda"])
  <|> (mkExprLet ("let" *> clause)  ("in" *> expr) <?> ["let"])
  <|> (mkExprIf  ("if" *> expr) ("then" *> expr) ("else" *> expr) <?> ["if-else"])
  <|> (mkExprCase ("case" *> expr) ("of" *> caseClauses) <?> ["case"])
  <|> (funApp <?> ["application"])
  )
-- singleApp :: Parsec (Expr -> Expr -> Expr)
-- singleApp = 
