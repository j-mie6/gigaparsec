{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Errors.Combinator (module Text.Gigaparsec.Errors.Combinator) where

import Text.Gigaparsec (Parsec)
-- We want to use this to make the docs point to the right definition for users.
--import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec))
import Text.Gigaparsec.Internal.Require (require)

import Data.Set (Set)

-- the empty set is weird here, do we require non-empty or just make it id?
label :: Set String -> Parsec a -> Parsec a
label ls = require (not (any null ls)) "labels cannot be empty" id --TODO:

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a -> Set String -> Parsec a
(<?>) = flip label
