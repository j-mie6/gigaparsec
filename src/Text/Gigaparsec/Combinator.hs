{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Combinator (module Text.Gigaparsec.Combinator) where

import Text.Gigaparsec (Parsec, void, many, some)
import Data.Foldable (asum)

choice :: [Parsec a] -> Parsec a
choice = asum

skipMany :: Parsec a -> Parsec ()
skipMany = void . many

skipSome :: Parsec a -> Parsec ()
skipSome = void . some
