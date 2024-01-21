{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Position (Pos, line, col, pos, offset, withWidth) where

import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), line, col, consumed)
import Control.Applicative (liftA2, liftA3)

type Pos :: *
type Pos = (Word, Word)

line :: Parsec Word
line = Internal.Parsec $ \st good _ -> good (Internal.line st) st

col :: Parsec Word
col = Internal.Parsec $ \st good _ -> good (Internal.col st) st

offset :: Parsec Word
offset = Internal.Parsec $ \st good _ -> good (Internal.consumed st) st

pos :: Parsec Pos
pos = liftA2 (,) line col

withWidth :: Parsec a -> Parsec (a, Word)
withWidth p = liftA3 (\s x e -> (x, e-s)) offset p offset
