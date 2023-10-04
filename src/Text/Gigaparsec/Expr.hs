{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
module Text.Gigaparsec.Expr (module Text.Gigaparsec.Expr) where

import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Expr.Infix (infixl1, infixr1, infixn1, prefix, postfix)
import Text.Gigaparsec.Expr.Subtype (Subtype(upcast))

import Data.List (foldl')

type Fixity :: * -> * -> * -> *
data Fixity a b sig where
  InfixL  :: Fixity a b (b -> a -> b)
  InfixR  :: Fixity a b (a -> b -> b)
  InfixN  :: Fixity a b (a -> a -> b)
  Prefix  :: Fixity a b (b -> b)
  Postfix :: Fixity a b (b -> b)

type Op :: * -> * -> *
data Op a b = forall sig. Op (Fixity a b sig) (a -> b) (Parsec sig)

type Prec :: * -> *
data Prec a where
  Level :: Prec a -> Op a b -> Prec b
  Atom  :: Parsec a -> Prec a

infixl 5 >+
(>+) :: Prec a -> Op a b -> Prec b
(>+) = Level

infixr 5 +<
(+<) :: Op a b -> Prec a -> Prec b
(+<) = flip (>+)

precedence :: Prec a -> Parsec a
precedence (Atom atom) = atom
precedence (Level lvls lvl) = con (precedence lvls) lvl
  where con :: Parsec a -> Op a b -> Parsec b
        con p (Op InfixL wrap op) = infixl1 wrap p op
        con p (Op InfixR wrap op) = infixr1 wrap p op
        con p (Op InfixN wrap op) = infixn1 wrap p op
        con p (Op Prefix wrap op) = prefix wrap op p
        con p (Op Postfix wrap op) = postfix wrap p op

precedence' :: Parsec a -> [Op a a] -> Parsec a
precedence' atom = precedence . foldl' (>+) (Atom atom)

gops :: Fixity a b sig -> (a -> b) -> [Parsec sig] -> Op a b
gops fixity wrap = Op fixity wrap . choice

ops :: Fixity a a sig -> [Parsec sig] -> Op a a
ops fixity = gops fixity id

sops :: Subtype a b => Fixity a b sig -> [Parsec sig] -> Op a b
sops fixity = gops fixity upcast
