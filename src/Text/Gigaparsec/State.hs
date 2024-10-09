{-# LANGUAGE Safe #-}
{-# LANGUAGE BlockArguments #-}
{-|
Module      : Text.Gigaparsec.State
Description : This module contains all the functionality and operations for using and manipulating references.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains all the functionality and operations for using and manipulating references.

These often have a role in performing context-sensitive parsing tasks, where a Turing-powerful system is required. 
Whilst a generic state monad is capable of such parsing, it is much less efficient than the use of references, though slightly more flexible. 
-}
module Text.Gigaparsec.State (
    {-| === References
    The Ref type to describes pieces of state that are threaded through a parser. 
    The creation and basic combinators of references are also found here.
    -}
    Ref,
    make, unsafeMake,
    get, gets,
    set, sets,
    update,
    updateDuring, setDuring,
    rollback,
    {-| === Reference-Based Combinators
    The following are variants of "Text.Gigaparsec.Combinator" combinators, made much more efficient using references.
    -}
    forP, forP', forP_, forP'_
  ) where

import Text.Gigaparsec (Parsec, (<|>), empty)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(..))

import Text.Gigaparsec.Combinator (ifS, whenS)

import Data.Ref (Ref, newRef, readRef, writeRef)

unsafeMake :: (forall r. Ref r a -> Parsec b) -> Parsec b
unsafeMake = make (error "reference used but not set")

_make :: Parsec a -> (forall r. Ref r a -> Parsec b) -> Parsec b
_make p f = p >>= \x -> make x f

make :: a -> (forall r. Ref r a -> Parsec b) -> Parsec b
make x f = Internal.Parsec $ \st good bad ->
  newRef x $ \ref ->
    let Internal.Parsec p = f ref
    in p st good bad

get :: Ref r a -> Parsec a
get ref = Internal.Parsec $ \st good _ ->
  do x <- readRef ref
     good x st

-- parsley provides multiple overloadings...
_gets :: Ref r a -> Parsec (a -> b) -> Parsec b
_gets ref pf = pf <*> get ref

gets :: Ref r a -> (a -> b) -> Parsec b
gets ref f = f <$> get ref

_set :: Ref r a -> Parsec a -> Parsec ()
_set ref px = px >>= set ref

set :: Ref r a -> a -> Parsec ()
set ref x = Internal.Parsec $ \st good _ ->
  do writeRef ref x
     good () st

sets :: Ref r b -> (a -> b) -> Parsec a -> Parsec ()
sets ref f px = _set ref (f <$> px)

_update :: Ref r a -> Parsec (a -> a) -> Parsec ()
_update ref pf = _set ref (_gets ref pf)

update :: Ref r a -> (a -> a) -> Parsec ()
update ref f = _set ref (gets ref f)

updateDuring :: Ref r a -> (a -> a) -> Parsec b -> Parsec b
updateDuring ref f p = do x <- get ref
                          set ref (f x)
                          p <* set ref x

setDuring :: Ref r a -> a -> Parsec b -> Parsec b
setDuring ref x = updateDuring ref (const x)

_setDuring :: Ref r a -> Parsec a -> Parsec b -> Parsec b
_setDuring ref px q = px >>= flip (setDuring ref) q

rollback :: Ref r a -> Parsec b -> Parsec b
rollback ref p = get ref >>= \x -> p <|> (set ref x *> empty)

forP :: Parsec a -> Parsec (a -> Bool) -> Parsec (a -> a) -> Parsec b -> Parsec [b]
forP ini cond step = forP' ini cond step . const

forP' :: Parsec a -> Parsec (a -> Bool) -> Parsec (a -> a) -> (a -> Parsec b) -> Parsec [b]
forP' ini cond step body = ini >>= go
  where go i = flip (ifS (cond <*> pure i)) (pure []) do
                  x <- body i
                  f <- step
                  xs <- go (f i)
                  return (x : xs)

forP_ :: Parsec a -> Parsec (a -> Bool) -> Parsec (a -> a) -> Parsec b -> Parsec ()
forP_ ini cond step = forP'_ ini cond step . const

forP'_ :: Parsec a -> Parsec (a -> Bool) -> Parsec (a -> a) -> (a -> Parsec b) -> Parsec ()
forP'_ ini cond step body = ini >>= go
  where go i = whenS (cond <*> pure i) do
                  body i
                  f <- step
                  go (f i)
