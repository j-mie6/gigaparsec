
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances     #-}
module ExprLang.AST where
import qualified Text.Gigaparsec.Token.Descriptions  as Desc
import qualified Text.Gigaparsec.Token.Lexer         as Lexer

import           Data.Char                           (generalCategory, isAlpha,
                                                      isAlphaNum, isSpace)
import qualified Data.Char                           as Char (GeneralCategory (Space))
import           Data.Int                            (Int64)
import           Data.List.NonEmpty                  (NonEmpty)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.String                         (IsString (fromString))
import           Data.Word                           (Word64)
import           Text.Gigaparsec                     (Parsec, Result(..),
                                                      parseFromFile, (<|>), ($>))
import           Text.Gigaparsec.Char                (newline, string, satisfy)
import           Text.Gigaparsec.Combinator           (endBy1, optional)
import           Text.Gigaparsec.Expr                (Fixity (InfixL),
                                                      Prec (Atom), precedence,
                                                      sops, (+<))
import           Text.Gigaparsec.Patterns            (deriveDeferredConstructors,
                                                      deriveLiftedConstructors)
import           Text.Gigaparsec.Token.Errors        (Bits)
import           Text.Gigaparsec.Token.Patterns      (overloadedStrings)
import Data.List (intercalate, intersperse)
type Var = String

data BinOp =
    OpAdd
  | OpMult
  | OpSubtract
  | OpDiv
  deriving stock (Show, Eq)

$(deriveLiftedConstructors "mk" ['OpAdd, 'OpMult, 'OpSubtract, 'OpDiv])



data Atom =
    AtomVar Var
  | AtomInt Int64
  deriving stock (Show)

data Expr =
    ExprBin BinOp Expr Expr
  | ExprAtom Atom
  deriving stock (Show)

$(deriveDeferredConstructors "mk" ['ExprBin, 'ExprAtom])

pattern ExprBinInfix :: Expr -> BinOp -> Expr -> Expr
pattern ExprBinInfix x op y = ExprBin op x y

newtype Return = Return Expr
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['Return])

data Stat = StatAssign Var Expr
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['StatAssign])
-- data Stat =
--     StatReturn Expr
--   | StatAssign Var Expr

data Program = Program [Stat] Return
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['Program])


class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty = id

instance Pretty Atom where
  pretty (AtomVar x) = pretty x
  pretty (AtomInt n) = show n

instance Pretty BinOp where
  pretty OpAdd = "+"
  pretty OpMult = "*"
  pretty OpSubtract = "-"
  pretty OpDiv = "/"

data ParenLevel =
    PAdd
  | PMult
  | PAtom
  deriving (Show, Eq, Ord)


opParenLevel :: BinOp -> ParenLevel
opParenLevel OpAdd = PAdd
opParenLevel OpSubtract = PAdd
opParenLevel OpMult = PMult
opParenLevel OpDiv = PMult

instance Pretty Expr where
  pretty e = prettyP PAtom e
    where
    prettyP p (ExprAtom x)     = pretty x
    prettyP p (ExprBin op x y) =
      let q = opParenLevel op
      in  withParen p q $ 
        concat [prettyP q x, " ", pretty op, " ", prettyP q y]

    withParen outer inner p 
      | outer < inner = "(" ++ p ++ ")"
      | otherwise     = p

instance Pretty Stat where
  pretty (StatAssign x e) =
    concat [pretty x, " = ", pretty e]

instance Pretty Return where
  pretty (Return e) =
    "return " ++ pretty e

instance Pretty Program where
  pretty (Program xs x) = concat [
      intercalate "\n" (map pretty xs)
    , if null xs then "" else "\n"
    , pretty x
    , "\n"
    ]
