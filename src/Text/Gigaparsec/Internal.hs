{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}
{-|
Module      : Text.Gigaparsec.Internal
Description : Internals of Gigaparsec
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : unstable

This module does __not__ adhere to PVP, and can change at any time as
required by the maintainers of the library. Use this functionality at your
own risk.

@since 0.1.0.0
-}
module Text.Gigaparsec.Internal (module Text.Gigaparsec.Internal) where

import Control.Applicative (Applicative(liftA2), Alternative(empty, (<|>), many, some)) -- liftA2 required until 9.6
import Control.Selective (Selective(select))

{-
Notes:

We are making a stripped back implementation, where there are way fewer generalisations
on the type: for now, no monad transformers, generalised input types, etc etc.
For consistency with other libraries, this is usually called `Parsec`.

Experimentally, it seems like dual-continuation implementations may be
faster than quad-continuation implementations. This will need some more
investigation and benchmarking to be sure about this however. We'll get a
core representation settled before doing any "hard" work (the composite
combinator API, however, can be done whenever).
-}
type Parsec :: * -> *
newtype Parsec a = Parsec {
    unParsec :: forall r. State
             -> (a -> State -> r) -- the good continuation
             -> (State -> r)      -- the bad continuation
             -> r
  }

deriving stock instance Functor Parsec -- not clear if there is a point to implementing this

instance Applicative Parsec where
  pure :: a -> Parsec a
  pure = undefined -- TODO:

  liftA2 :: (a -> b -> c) -> Parsec a -> Parsec b -> Parsec c
  liftA2 = undefined -- TODO:

  (*>) :: Parsec a -> Parsec b -> Parsec b
  (*>) = liftA2 (const id)

  (<*) :: Parsec a -> Parsec b -> Parsec a
  (<*) = liftA2 const

  {-# INLINE pure #-}
  {-# INLINE liftA2 #-}
  {-# INLINE (<*) #-}
  {-# INLINE (*>) #-}

instance Selective Parsec where
  select :: Parsec (Either a b) -> Parsec (a -> b) -> Parsec b
  select p q = _branch p q (pure id)

  {-# INLINE select #-}

{-# INLINE _branch #-}
{-|
This is an internal implementation of `branch`, which is more efficient than
the Selective default `branch`. We should be using this internally, and it
can be dropped if https://github.com/snowleopard/selective/issues/74 is implemented.
-}
_branch :: Parsec (Either a b) -> Parsec (a -> c) -> Parsec (b -> c) -> Parsec c
_branch = undefined -- TODO:

instance Monad Parsec where
  return :: a -> Parsec a
  return = pure

  (>>=) :: Parsec a -> (a -> Parsec b) -> Parsec b
  (>>=) = undefined -- TODO:

  (>>) :: Parsec a -> Parsec b -> Parsec b
  (>>) = (*>)

  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance Alternative Parsec where
  empty :: Parsec a
  empty = undefined -- TODO:

  (<|>) :: Parsec a -> Parsec a -> Parsec a
  (<|>) = undefined -- TODO:

  many :: Parsec a -> Parsec [a]
  many p = let go = liftA2 (:) p go <|> pure [] in go

  some :: Parsec a -> Parsec [a]
  some p = liftA2 (:) p (many p)

  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}
  {-# INLINE many #-}
  {-# INLINE some #-}

type State :: *
data State = State {
    -- | the input string, in future this may be generalised
    input :: !String,
    -- | has the parser consumed input since the last relevant handler?
    consumed :: !Bool, -- this could be an Int offset instead, perhaps?
    -- | the current line number (incremented by \n)
    line :: {-# UNPACK #-} !Int,
    -- | the current column number (have to settle on a tab handling scheme)
    col  :: {-# UNPACK #-} !Int
  }

emptyState :: String -> State
emptyState !str = State { input = str
                        , consumed = False
                        , line = 1
                        , col = 1
                        }
