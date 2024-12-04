{-# LANGUAGE TemplateHaskell #-}
module Haskell.AST where

import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec.Position (Pos)
import Text.Gigaparsec.Patterns


type Name = String
type TName = String
type TVar = String
type OpName = String

type Cstr = String

data Atom = 
    AtomInt Integer
  | AtomVar Name
  | AtomCstr Cstr
  deriving (Show, Eq)

newtype BinOp = 
  BinOp OpName
  deriving (Show, Eq)

{-

Notes:

* Unary ops are handled by application
-}

data Expr = 
    ExprAtom Pos Atom
  | ExprLam  Pos (NonEmpty Pattern) Expr
  | ExprLet  Pos Clause Expr
  | ExprIf   Pos Expr Expr Expr
  | ExprTuple Pos [Expr]
  | ExprApp  Pos Expr (NonEmpty Expr)
  | ExprBin  Pos BinOp Expr Expr
  | ExprCase Pos Expr [(Pattern, Expr)]
  | ExprParen Pos Expr
  deriving (Show, Eq)


data TBinOp = 
    TArr
  deriving (Show, Eq)

data Type = 
    TypeVar Pos TVar
  | TypeData Pos TName
  | TypeStar Pos 
  | TypeArr Pos Type Type
  | TypeForall Pos (NonEmpty TVar) Type
  | TypeApp Pos Type Type
  | TypeBin Pos TBinOp Type Type
  | TypeTuple Pos Type (NonEmpty Type)
  | TypeParen Type
  deriving (Show, Eq)



data Pattern = 
    PatVar Pos Name
  | PatCon Pos Cstr [Pattern] 
  deriving (Show, Eq)

data Clause = Clause Name [Pattern] Expr
  deriving (Show, Eq)

data Param = Param TVar Type
  deriving (Show, Eq)

data CstrDef = CstrDef Cstr [Type]
  deriving (Show, Eq)

data Decl = 
    DFun Pos Name [Pattern] Expr [Decl]
  | DSig Pos Name Type
  | DData Pos TName [Param] [CstrDef]
  deriving (Show, Eq)

data Program = Program String [Decl]

$(deriveLiftedConstructors "mk" 
  ['TypeVar, 'TypeData, 'TypeStar, 'TypeArr, 'TypeForall, 'TypeApp, 'TypeBin, 'TypeTuple, 'TypeParen])
$(deriveLiftedConstructors "mk" ['Clause])


$(deriveDeferredConstructors "mk" ['ExprAtom, 'ExprApp, 'ExprBin])
$(deriveLiftedConstructors "mk" ['ExprLam, 'ExprLet, 'ExprIf, 'ExprCase, 'ExprTuple, 'ExprParen])
$(deriveLiftedConstructors "mk" ['PatVar, 'PatCon])
