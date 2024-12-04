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

instance ToHaskell Atom where
  toHsSb = sbs . \case
    AtomInt x -> show x
    AtomVar x -> x
    AtomCstr x -> x

instance ToHaskell BinOp where
  toHsSb = sbs . \case
    BinOp x -> x

indent = ("  " <>)

indentn :: Functor f => f StringBuilder -> f StringBuilder
indentn = fmap ("  " <>)

indentTail [] = []
indentTail (x: xs) = x: fmap indent xs

attachHead :: StringBuilder -> [StringBuilder] -> [StringBuilder]
attachHead e [] = [e]
attachHead e (x: xs) = (e <> x) : xs

commas :: [StringBuilder] -> [StringBuilder]
commas [] = []
commas [x] = [x]
commas (x: xs) = (x <> ","): xs

expr :: Expr -> StringBuilder
expr = expr' 0
  where
  expr' :: Int -> Expr -> StringBuilder
  expr' i = \case
    ExprAtom _ a -> toHsSb a
    ExprLam _ ps t ->
            "λ" <> patternsNE ps <> " → " (expr' i t)
    ExprLet _ cls t ->
            attachHead "let " (clause cls)
        ++ attachHead "\nin   " (expr' (i + 1) t)
    ExprIf _ c t f ->
          "if " <> expr' i c
      <>  "\n  then " <> expr' (i + 1) t
      <>  "\n  else " <> expr' (i + 1) f
    ExprTuple _ ts ->
          "(" <> mconcat (commas $ map (expr' i )ts) <> ")"
    ExprApp _ t us ->
      mconcat (expr t :NE.toList (fmap ((" " <>) . expr' i) us))
    ExprBin _ op t u -> "(" <> expr' i t <> ") " <> op " (" <> expr' i u ")"
    ExprCase _ e cs -> _
    ExprParen _ t -> "(" <> expr t <> ")"

instance ToHaskell Expr where
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

paren x = "(" <> x <> ")"

patternsNE :: NonEmpty Pattern -> StringBuilder
patternsNE ps = mconcat $ NE.toList (fmap (paren . pattern_) ps)

patterns :: [Pattern] -> StringBuilder
patterns = mconcat . fmap (paren . pattern_)


clause :: Clause -> [StringBuilder]
clause (Clause x ps e) =
  attachHead (sb (show x) <> patterns ps <> " = ") (toHsSb e)

param :: Param -> StringBuilder
param (Param a tp) = sb (show a) <> " :: " <> type_ tp

type_ :: Type -> StringBuilder
type_ tp = undefined

instance ToHaskell CstrDef where

instance ToHaskell Decl where
