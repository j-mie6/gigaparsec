{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Haskell.AST.ToHaskell where

import Haskell.AST

import GHC.Exts ( IsString(..), IsList, IsList(..) )
import Data.List (intercalate, intersperse)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable qualified as Foldable

newtype StringBuilder = StringBuilder { unStringBuilder :: (String -> String) }
  deriving newtype (Semigroup, Monoid)

instance Show StringBuilder where
  show :: StringBuilder -> String
  show (StringBuilder x) = x ""

buildString :: StringBuilder -> String
buildString (StringBuilder sb) = sb ""

sb :: String -> StringBuilder
sb x = StringBuilder (x ++)

_to :: ToHaskell a => a -> [StringBuilder]
_to = toHsSb

singleton x = [x]

sbs = singleton . sb

instance IsString StringBuilder where
  fromString :: String -> StringBuilder
  fromString = sb

class ToHaskell a where
  toHsSb :: a -> [StringBuilder]

  toHs :: a -> String
  toHs = buildString . mconcat . map ("\n"<>) . toHsSb

  toHaskell :: a -> String
  toHaskell = toHs

atom :: Atom -> StringBuilder
atom = sb . \case
    AtomInt x -> show x
    AtomVar x -> x
    AtomCstr x -> x

instance ToHaskell Atom where
  toHsSb :: Atom -> [StringBuilder]
  toHsSb = singleton . atom
instance ToHaskell BinOp where
  toHsSb :: BinOp -> [StringBuilder]
  toHsSb = sbs . \case
    BinOp x -> x

binOp :: BinOp -> StringBuilder
binOp (BinOp x) = sb x

indent :: StringBuilder -> StringBuilder
indent = ("  " <>)

indentn :: Functor f => f StringBuilder -> f StringBuilder
indentn = fmap ("  " <>)

indentTail :: [StringBuilder] -> [StringBuilder]
indentTail [] = []
indentTail (x: xs) = x: fmap indent xs

attachHead :: StringBuilder -> [StringBuilder] -> [StringBuilder]
attachHead e [] = [e]
attachHead e (x: xs) = (e <> x) : xs

mintercalate :: Semigroup m => m -> [m] -> [m]
mintercalate m [] = []
mintercalate m [x] = [x]
mintercalate m (x: xs) = (x <> m): xs

commas :: [StringBuilder] -> [StringBuilder]
commas = mintercalate ", "
expr :: Expr -> StringBuilder
expr = \case
  ExprAtom _ a -> atom a
  ExprLam _ ps t ->
          "λ" <> patternsNE ps <> " → " <> expr t
  ExprLet _ cls t ->
          "let " <> clause cls <> "in   " <> expr t
  ExprIf _ c t f ->
        "if " <> expr c
    <>  "\n  then " <> expr t
    <>  "\n  else " <> expr f
  ExprTuple _ ts ->
        "(" <> mconcat (commas $ map expr ts) <> ")"
  ExprApp _ t us ->
    mconcat (expr t :NE.toList (fmap ((" " <>) . paren . expr) us))
  ExprBin _ op t u -> mconcat ["(", expr t, ") ", binOp op, " (", expr u, ")"]
  ExprCase _ e cs -> mconcat ["case ", expr e, " of {"
    , mconcat $ mintercalate ";" $ map (\(p, c) -> pattern_ p <> " -> " <> expr c) cs
    , "}"]
  ExprParen _ t -> "(" <> expr t <> ")"

instance ToHaskell Expr where
  toHsSb :: Expr -> [StringBuilder]
  toHsSb = singleton . expr
  -- toHsSb = \case
  --   ExprAtom _ a -> toHsSb a
  --   ExprLam _ ps t ->
  --           "λ" <> patternsNE ps
  --         : attachHead " → " (toHsSb t)
  --   ExprLet _ cls t ->
  --          attachHead "let " (clause cls)
  --       ++ attachHead "in  " (toHsSb t)
  --   ExprIf _ c t f ->
  --         attachHead "if " (toHsSb c)
  --     ++  attachHead "then " (toHsSb t)
  --     ++  attachHead "else " (toHsSb f)
  --   ExprTuple _ ts ->
  --         attachHead "(" (commas $ concatMap toHsSb ts) ++ [")"]
  --   ExprApp _ t us ->
  --     [mconcat (mconcat (toHsSb t) : NE.toList (fmap ((" " <>) . mconcat . toHsSb) us))]

pattern_ :: Pattern -> StringBuilder
pattern_ = \case
  PatVar _ x -> sb x
  PatCon _ c ps -> sb c <> mconcat (map pattern_ ps)

paren :: (Semigroup a, IsString a) => a -> a
paren x = "(" <> x <> ")"

patternsNE :: NonEmpty Pattern -> StringBuilder
patternsNE ps = mconcat $ NE.toList (fmap (paren . pattern_) ps)

patterns :: [Pattern] -> StringBuilder
patterns = mconcat . fmap (paren . pattern_)


clause :: Clause -> StringBuilder
clause (Clause x ps e) =
  sb (show x) <> patterns ps <> " = " <> expr e

param :: Param -> StringBuilder
param (ParamVar a) = sb (show a)
param (ParamSig a tp) = paren $ sb (show a) <> " :: " <> type_ tp

params :: Foldable t => t Param -> StringBuilder
params ps = mconcat $ spaces (map param $ Foldable.toList ps)

tbinOp :: IsString p => TBinOp -> p
tbinOp TArr = "->"

type_ :: Type -> StringBuilder
type_ = \case
  TypeVar _ s -> sb s
  TypeData _ s -> sb s
  TypeStar _ -> "*"
  TypeArr _ a b -> paren (type_ a) <> " -> " <> type_ b
  TypeForall _ ps a -> "∀ " <> params ps <> ". " <> type_ a
  TypeApp _ a b -> type_ a <> " " <> type_ b
  TypeBin _ op a b -> mconcat [type_ a, " ", tbinOp op, " ", type_ b]
  TypeTuple _ a bs -> paren $ mconcat $ commas (type_ a: map type_ (NE.toList bs))
  TypeParen a -> "(" <> type_ a <> ")"

spaces :: [StringBuilder] -> [StringBuilder]
spaces = mintercalate " "

guards :: [StringBuilder] -> [StringBuilder]
guards = mintercalate "|"

instance ToHaskell CstrDef where
  toHsSb :: CstrDef -> [StringBuilder]
  toHsSb = singleton . cstrDef

cstrDef :: CstrDef -> StringBuilder
cstrDef (CstrDef c ts) = mconcat $ spaces (sb c: map type_ ts)

decl :: Decl -> [StringBuilder]
decl = \case
  DFun _ f pats t wheres -> sb f <> patterns pats <> expr t:
    if null wheres then []
      else "  where": concatMap decl wheres
  DSig _ f tp -> [sb f <> " :: " <> type_ tp]
  DData _ tp ps cds -> mconcat ["data ", sb tp, params ps, " = "] :
    guards (map cstrDef cds)


instance ToHaskell Decl where
  toHsSb :: Decl -> [StringBuilder]
  toHsSb = decl
