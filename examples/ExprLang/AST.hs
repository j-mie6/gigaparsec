{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-|
This module defines the AST (abstract syntax tree) for 'ExprLang'.
There are also a few functions for dealing with the AST, including a pretty printing function 'pretty'.
-}
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

{-|
A `BinOp` is a binary operator, which appears in binary expressions.
-}
data BinOp =
    OpAdd
  | OpMult
  | OpSubtract
  | OpDiv
  deriving stock (Show, Eq)

$(deriveLiftedConstructors "mk" ['OpAdd, 'OpMult, 'OpSubtract, 'OpDiv])


{-| These are the most basic expressions, consisting of variables and integers.
-}
data Atom =
    AtomVar Var
  | AtomInt Int64
  deriving stock (Show)

{-| 'Expr'essions are formed in terms of 'Atom's and binary expressions,
in which a 'BinOp' is applied to two subexpressions.
-}
data Expr =
    ExprBin BinOp Expr Expr
  | ExprAtom Atom
  deriving stock (Show)

$(deriveDeferredConstructors "mk" ['ExprBin, 'ExprAtom])

-- | An infix version of the `ExprBin` constructor.
pattern ExprBinInfix :: Expr -> BinOp -> Expr -> Expr
pattern ExprBinInfix x op y = ExprBin op x y

-- | The final statement is always a `Return` statement, which returns the result of an expression.
newtype Return = Return Expr
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['Return])

-- | A 'Stat'ement assigns a variable to the value of an expression.
data Stat = StatAssign Var Expr
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['StatAssign])

-- | A 'Program' consists of a series of statements (assignments) and a final return clause.
-- The expression returned may be defined in terms of the variables defined in the preceding statements.
data Program = Program [Stat] Return
  deriving stock (Show)

$(deriveLiftedConstructors "mk" ['Program])

{-|
A type @a@ is 'Pretty' when there is a way to generate a pretty string from @a@.
-}
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

{-|
For pretty-printing, we define different 'precedence levels', which help us determine
whether or not a subexpression should be parenthesised
-}
data ParenLevel =
    PAdd  -- ^ The lowest precedence: for addition/subtraction
  | PMult -- ^ The middle precedence: for multiplication/division
  | PAtom -- ^ The highest precedence: atoms will never need parentheses.
  deriving (Show, Eq, Ord)

-- | Determines the precedence level of a given `BinOp`.
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
