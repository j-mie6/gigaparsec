{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Combinator (module Text.Gigaparsec.Combinator) where

import Text.Gigaparsec (Parsec, void, many, some, (<|>), ($>), (<:>))
import Data.Foldable (asum)

choice :: [Parsec a] -> Parsec a
choice = asum

skipMany :: Parsec a -> Parsec ()
skipMany = void . many

skipSome :: Parsec a -> Parsec ()
skipSome = void . some

manyTill :: Parsec a -> Parsec end -> Parsec [a]
manyTill p end = let go = end $> [] <|> p <:> go in go

sepEndBy1 :: Parsec a -> Parsec sep -> Parsec [a]
sepEndBy1 p sep = let seb1 = p <:> (sep *> (seb1 <|> pure []) <|> pure []) in seb1
