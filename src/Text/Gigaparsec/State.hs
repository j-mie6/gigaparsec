{-# LANGUAGE Safe #-}
module Text.Gigaparsec.State (
    Ref,
    make, unsafeMake,
    get, gets,
    set, sets,
    update,
    local, localWith,
    rollback
  ) where

import Text.Gigaparsec (Parsec, (<|>), empty)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(..))

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

local :: Ref r a -> (a -> a) -> Parsec b -> Parsec b
local ref f p = do x <- get ref
                   set ref (f x)
                   p <* set ref x

localWith :: Ref r a -> a -> Parsec b -> Parsec b
localWith ref x = local ref (const x)

_localWith :: Ref r a -> Parsec a -> Parsec b -> Parsec b
_localWith ref px q = px >>= flip (localWith ref) q

rollback :: Ref r a -> Parsec a -> Parsec a
rollback ref p = get ref >>= \x -> p <|> (set ref x *> empty)

-- TODO: for combinators
