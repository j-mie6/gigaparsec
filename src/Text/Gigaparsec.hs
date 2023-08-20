{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO: module header, and categories in the export (see https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/parsley/Parsley.html)
module Text.Gigaparsec (Parsec, Result(..), parse, atomic, lookAhead, notFollowedBy, label, (<?>)) where

-- NOTE:
-- This module is mostly just for re-exports, though there may be primitive
-- combinators in here too. If user can see it, it can go in here.
--
-- Care MUST be taken to not expose /any/ implementation details from
-- `Internal`: when they are in the public API, we are locked into them!

import Text.Gigaparsec.Internal (unParsec, emptyState)
import Text.Gigaparsec.Internal qualified as Internal (Parsec)
import Text.Gigaparsec.Internal.Require (require)

import Data.Set (Set)

-- re-expose like this to prevent hlint suggesting import refinement into internal
type Parsec = Internal.Parsec

type Result :: * -> *
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

-- the empty set is weird here, do we require non-empty or just make it id?
label :: Set String -> Parsec a -> Parsec a
label ls = require (not (any null ls)) "Labels cannot be empty" id

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a -> Set String -> Parsec a
(<?>) = flip label
