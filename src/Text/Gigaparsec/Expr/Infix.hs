{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Expr.Infix (module Text.Gigaparsec.Expr.Infix) where

import Text.Gigaparsec (Parsec, (<|>), (<**>))

infixl1 :: (a -> b) -> Parsec a -> Parsec (b -> a -> b) -> Parsec b
infixl1 wrap p op = postfix wrap p (flip <$> op <*> p)

infixr1 :: (a -> b) -> Parsec a -> Parsec (a -> b -> b) -> Parsec b
infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

infixn1 :: (a -> b) -> Parsec a -> Parsec (a -> a -> b) -> Parsec b
infixn1 wrap p op = p <**> (flip <$> op <*> p <|> pure wrap)

prefix :: (a -> b) -> Parsec (b -> b) -> Parsec a -> Parsec b
prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

postfix :: (a -> b) -> Parsec a -> Parsec (b -> b) -> Parsec b
postfix wrap p op = wrap <$> p <**> rest
  where rest = flip (.) <$> op <*> rest <|> pure id
