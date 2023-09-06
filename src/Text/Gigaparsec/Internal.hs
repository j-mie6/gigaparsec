{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
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

import Text.Gigaparsec.Internal.RT (RT)

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
             -> (a -> State -> RT r) -- the good continuation
             -> (State -> RT r)      -- the bad continuation
             -> RT r
  }

deriving stock instance Functor Parsec -- not clear if there is a point to implementing this

instance Applicative Parsec where
  pure :: a -> Parsec a
  pure x = Parsec $ \st ok _ -> ok x st
  -- Continue with x and no input consumed.

  liftA2 :: (a -> b -> c) -> Parsec a -> Parsec b -> Parsec c
  liftA2 f (Parsec p) (Parsec q) = Parsec $ \st ok err ->
    let ok' x st' = q st' (ok . f x) err
    --                    ^^^^^^^^^^
    -- continue with (f x y), where y is the output of q
    in  p st ok' err

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
_branch (Parsec p) (Parsec q1) (Parsec q2) = Parsec $ \st ok err ->
  let ok' x st' = case x of
        Left a  -> q1 st' (ok . ($ a)) err
        --                ^^^^^^^^^^^^
        Right b -> q2 st' (ok . ($ b)) err
        --                ^^^^^^^^^^^^
        -- feed a/b to the function of the good continuation
  in  p st ok' err

instance Monad Parsec where
  return :: a -> Parsec a
  return = pure

  (>>=) :: Parsec a -> (a -> Parsec b) -> Parsec b
  Parsec p >>= f = Parsec $ \st ok err ->
    let ok' x st' = unParsec (f x) st' ok err
    --              ^^^^^^^^^^^^^^
    -- get the parser obtained from feeding the output of p to f
    in  p st ok' err

  (>>) :: Parsec a -> Parsec b -> Parsec b
  (>>) = (*>)

  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance Alternative Parsec where
  empty :: Parsec a
  empty = Parsec $ \st _ err -> err st

  (<|>) :: Parsec a -> Parsec a -> Parsec a
  Parsec p <|> Parsec q = Parsec $ \st ok err ->
    let !initConsumed = consumed st
        ok' x st' = ok x (st' { consumed = initConsumed || consumed st' })
          --  ^ revert to old st.consumed if p didn't consume
        err' st'
          | consumed st' = err st'
          --  ^ fail if p failed *and* consumed
          | otherwise    = q (st' { consumed = initConsumed }) ok err

    in  p (st { consumed = False }) ok' err'

  many :: Parsec a -> Parsec [a]
  many p = let go = liftA2 (:) p go <|> pure [] in go

  some :: Parsec a -> Parsec [a]
  some p = liftA2 (:) p (many p)

  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}
  {-# INLINE many #-}
  {-# INLINE some #-}

instance Semigroup m => Semigroup (Parsec m) where
  (<>) :: Parsec m -> Parsec m -> Parsec m
  (<>) = liftA2 (<>)

  {-# INLINE (<>) #-}

instance Monoid m => Monoid (Parsec m) where
  mempty :: Parsec m
  mempty = pure mempty

  {-# INLINE mempty #-}

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
