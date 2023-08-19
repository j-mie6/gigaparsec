{-# LANGUAGE Safe #-}
-- TODO: module header, and categories in the export (see https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/parsley/Parsley.html)
module Text.Gigaparsec (Parsec, Result(..), parse, atomic, lookAhead, notFollowedBy) where

-- NOTE:
-- This module is mostly just for re-exports, though there may be primitive
-- combinators in here too. If user can see it, it can go in here.
--
-- Care MUST be taken to not expose /any/ implementation details from
-- `Internal`: when they are in the public API, we are locked into them!

import Text.Gigaparsec.Internal (Parsec(unParsec), emptyState)

data Result a = Success a | Failure deriving stock (Show, Eq)

parse :: Parsec a -> String -> Result a
parse p input = unParsec p (emptyState input) good bad
  where good x _ = Success x
        bad _    = Failure

atomic :: Parsec a -> Parsec a
atomic = undefined --TODO:

lookAhead :: Parsec a -> Parsec a
lookAhead = undefined --TODO:

notFollowedBy :: Parsec a -> Parsec ()
notFollowedBy = undefined --TODO:
