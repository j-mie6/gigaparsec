{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Registers (
    Reg,
    make, unsafeMake,
    get, gets,
    put, puts,
    modify,
    local, localWith,
    rollback
  ) where

import Text.Gigaparsec (Parsec, (<|>), empty)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(..))

import Data.Ref (Ref, newRef, readRef, writeRef)

type Reg :: * -> * -> *
type Reg = Ref

unsafeMake :: (forall r. Reg r a -> Parsec b) -> Parsec b
unsafeMake = make (error "reference used but not set")

_make :: Parsec a -> (forall r. Reg r a -> Parsec b) -> Parsec b
_make p f = p >>= \x -> make x f

make :: a -> (forall r. Reg r a -> Parsec b) -> Parsec b
make x f = Internal.Parsec $ \st good bad ->
  newRef x $ \reg ->
    let Internal.Parsec p = f reg
    in p st good bad

get :: Reg r a -> Parsec a
get reg = Internal.Parsec $ \st good _ ->
  do x <- readRef reg
     good x st

-- parsley provides multiple overloadings...
_gets :: Reg r a -> Parsec (a -> b) -> Parsec b
_gets reg pf = pf <*> get reg

gets :: Reg r a -> (a -> b) -> Parsec b
gets reg f = f <$> get reg

_put :: Reg r a -> Parsec a -> Parsec ()
_put reg px = px >>= put reg

put :: Reg r a -> a -> Parsec ()
put reg x = Internal.Parsec $ \st good _ ->
  do writeRef reg x
     good () st

puts :: Reg r b -> (a -> b) -> Parsec a -> Parsec ()
puts reg f px = _put reg (f <$> px)

_modify :: Reg r a -> Parsec (a -> a) -> Parsec ()
_modify reg pf = _put reg (_gets reg pf)

modify :: Reg r a -> (a -> a) -> Parsec ()
modify reg f = _put reg (gets reg f)

local :: Reg r a -> (a -> a) -> Parsec b -> Parsec b
local reg f p = do x <- get reg
                   put reg (f x)
                   p <* put reg x

localWith :: Reg r a -> a -> Parsec b -> Parsec b
localWith reg x = local reg (const x)

_localWith :: Reg r a -> Parsec a -> Parsec b -> Parsec b
_localWith reg px q = px >>= flip (localWith reg) q

rollback :: Reg r a -> Parsec a -> Parsec a
rollback reg p = get reg >>= \x -> p <|> (put reg x *> empty)

-- TODO: for combinators
