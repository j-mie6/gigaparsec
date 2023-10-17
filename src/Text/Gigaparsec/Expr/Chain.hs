{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Expr.Chain (module Text.Gigaparsec.Expr.Chain) where

import Text.Gigaparsec (Parsec, (<|>), (<**>))
import Text.Gigaparsec.Expr.Infix qualified as Infix (infixl1, infixr1, infixn1, prefix, postfix)

chainl1 :: Parsec a -> Parsec (a -> a -> a) -> Parsec a
chainl1 = Infix.infixl1 id

chainr1 :: Parsec a -> Parsec (a -> a -> a) -> Parsec a
chainr1 = Infix.infixr1 id

chainn1 :: Parsec a -> Parsec (a -> a -> a) -> Parsec a
chainn1 = Infix.infixn1 id

prefix :: Parsec (a -> a) -> Parsec a -> Parsec a
prefix = Infix.prefix id

postfix :: Parsec a -> Parsec (a -> a) -> Parsec a
postfix = Infix.postfix id

prefix1 :: (b -> a) -> Parsec (a -> b) -> Parsec a -> Parsec b
prefix1 wrap op p = op <*> prefix ((wrap .) <$> op) p

postfix1 :: (b -> a) -> Parsec a -> Parsec (a -> b) -> Parsec b
postfix1 wrap p op = postfix (p <**> op) ((. wrap) <$> op)

chainl :: Parsec a -> Parsec (a -> a -> a) -> a -> Parsec a
chainl p op x = chainl1 p op <|> pure x

chainr :: Parsec a -> Parsec (a -> a -> a) -> a -> Parsec a
chainr p op x = chainr1 p op <|> pure x
