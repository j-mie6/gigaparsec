{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- See
-- https://docs.python.org/3/reference/grammar.html

module SmallPython.AST where

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Map (Map)
import Data.Map qualified as Map
import Text.Gigaparsec.Position (Pos)
import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Position (pos)
import Data.Functor ((<&>))

type Name = String
type Params = [(Name, Maybe Expr)]
type TypeParams = ()


newtype ParseInfo = ParseInfo Pos
  deriving (Show, Eq)
{-# inline parseInfo #-}
parseInfo :: Parsec ParseInfo
parseInfo = ParseInfo <$> pos

newtype Program = Program Stats
  deriving (Show, Eq)

data AssignType = 
    AssignSimple
  | AssignPlus
  deriving (Show, Eq)

-- | A python program is a series of statements, 
-- which have some effect on the state or control flow of the program.
data Stat = 
    StatFunctionDef ParseInfo Name Params Stats1
  | StatPass ParseInfo
  | StatAssign ParseInfo AssignType ExprLHS Expr
  | StatReturn ParseInfo Expr
  | StatExp ParseInfo Expr
  deriving (Show, Eq)

type Stats = [Stat]
type Stats1 = NonEmpty Stat

mkFunctionDef :: Parsec Name -> Parsec Params -> Parsec Stats1 -> Parsec Stat
mkFunctionDef f xs body = StatFunctionDef <$> parseInfo <*> f <*> xs <*> body

mkStatExp :: Parsec Expr -> Parsec Stat
mkStatExp expr = StatExp <$> parseInfo <*> expr

mkStatAssign :: Parsec ExprLHS -> Parsec AssignType -> Parsec Expr -> Parsec Stat
mkStatAssign pLhs pAsgn pExpr = do
  info <- parseInfo
  lhs <- pLhs
  asgn <- pAsgn
  StatAssign info asgn lhs <$> pExpr

mkReturn :: Parsec Expr -> Parsec Stat
mkReturn x = StatReturn <$> parseInfo <*> x

-- mkStats :: Parsec (NonEmpty Stat) -> Parsec Stat
-- mkStats = (<&> \case
--   x :| [] -> x
--   xs -> StatSeq xs)

-- | Binary operators that may appear between two expressions
data BinOpSymbol = 
    BinPlus
  | BinMinus
  | BinMult
  | BinDiv
  | BinFloorDiv
  | BinExponent
  deriving (Show, Eq)

type BinOp = (ParseInfo, BinOpSymbol)

pattern BinOp :: ParseInfo -> BinOpSymbol -> BinOp
pattern BinOp p op = (p, op)

mkBinOp :: BinOpSymbol -> Parsec BinOp
mkBinOp op = (`BinOp` op) <$> parseInfo

-- | Unary prefix operators on expressions
data UnaryOpSymbol = 
    UnaryPlus
  | UnaryMinus
  deriving (Show, Eq)
type UnaryOp = (ParseInfo, UnaryOpSymbol)

pattern UnaryOp :: ParseInfo -> UnaryOpSymbol -> UnaryOp
pattern UnaryOp p op = (p, op)

mkUnaryOp :: UnaryOpSymbol -> Parsec UnaryOp
mkUnaryOp op = (`UnaryOp` op) <$> parseInfo


data AtomNumber =
    AtomInt    !Integer
  | AtomDouble !Double
  deriving (Show, Eq)

-- | Atoms are the most basic expressions; think of literals and variables.
data Atom =
    AtomNumber AtomNumber
  | AtomChar   !Char
  | AtomString !String
  | AtomVar    !Name
  deriving (Show, Eq)

mkAtomNumber :: Parsec AtomNumber -> Parsec Atom
mkAtomNumber x = AtomNumber <$> x

mkAtomVar :: Parsec Name -> Parsec Atom
mkAtomVar x = AtomVar <$> x


-- | Expressions: these occupy the rhs of statements.
data Expr = 
    ExprAtom  ParseInfo Atom
  | ExprBin   ParseInfo BinOp Expr Expr
  | ExprUnary ParseInfo UnaryOp Expr
  | ExprFunctionCall ParseInfo Expr [Expr]
  deriving (Show, Eq)

mkExprAtom :: Parsec Atom -> Parsec Expr
mkExprAtom = (ExprAtom <$> parseInfo <*>)

mkExprBin :: Parsec (BinOp -> Expr -> Expr -> Expr)
mkExprBin = ExprBin <$> parseInfo

mkFunctionCall :: Parsec Expr -> Parsec [Expr] -> Parsec Expr
mkFunctionCall p ps = ExprFunctionCall <$> parseInfo <*> p <*> ps

-- | Expressions that can appear on the LHS of assignment statements.
data ExprLHS = 
    LHSIdent Name
  deriving (Show, Eq)

-- TODO: need to test that all ops are covered
{-# noinline binOpMap #-}
binOpMap :: Map String BinOpSymbol
binOpMap = [
  ("+",  BinPlus),
  ("-",  BinMinus),
  ("*",  BinMult),
  ("/",  BinDiv),
  ("//", BinFloorDiv),
  ("**", BinExponent)
  ]

{-# noinline unaryOpMap #-}
unaryOpMap :: Map String UnaryOpSymbol
unaryOpMap = [
    ("+", UnaryPlus),
    ("-", UnaryMinus)
  ]
