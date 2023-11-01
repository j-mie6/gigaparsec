{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, RecordWildCards, NamedFieldPuns, CPP #-}
#include "portable-unlifted.h"
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
import Text.Gigaparsec.Internal.Errors (ParseError, ExpectItem, CaretWidth)
import Text.Gigaparsec.Internal.Errors qualified as Errors (
    emptyErr, expectedErr, specialisedErr, mergeErr, unexpectedErr,
    expecteds, isExpectedEmpty, presentationOffset, useHints
  )

import Control.Applicative (Applicative(liftA2), Alternative(empty, (<|>), many, some)) -- liftA2 required until 9.6
import Control.Selective (Selective(select))

import Data.Set (Set)
import Data.Set qualified as Set (empty, union)

CPP_import_PortableUnlifted

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
             -> (ParseError -> State -> RT r)      -- the bad continuation
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

raise :: (State -> ParseError) -> Parsec a
raise mkErr = Parsec $ \st _ bad -> useHints bad (mkErr st) st

instance Alternative Parsec where
  empty :: Parsec a
  empty = raise (`emptyErr` 0)

  -- FIXME: I feel like there is something missing here with hint merging from ctx.mergeHints
  -- if the hint stack is not real then it lives in the continuation trace, but I don't know... which
  (<|>) :: Parsec a -> Parsec a -> Parsec a
  Parsec p <|> Parsec q = Parsec $ \st ok bad ->
    let bad' err st'
          | consumed st' > consumed st = bad err st'
          --  ^ fail if p failed *and* consumed
          | otherwise    = q st' (\x st'' -> ok x (errorToHints st'' err))
                                 (\err' -> bad (Errors.mergeErr err err'))
    in  p st ok bad'

  many :: Parsec a -> Parsec [a]
  many = manyr (:) []

  some :: Parsec a -> Parsec [a]
  some = somer (:) []

  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}
  {-# INLINE many #-}
  {-# INLINE some #-}

{-# INLINE manyr #-}
manyr :: (a -> b -> b) -> b -> Parsec a -> Parsec b
manyr f k p = let go = liftA2 f p go <|> pure k in go

{-# INLINE somer #-}
somer :: (a -> b -> b) -> b -> Parsec a -> Parsec b
somer f k p = liftA2 f p (manyr f k p)

instance Semigroup m => Semigroup (Parsec m) where
  (<>) :: Parsec m -> Parsec m -> Parsec m
  (<>) = liftA2 (<>)

  {-# INLINE (<>) #-}

instance Monoid m => Monoid (Parsec m) where
  mempty :: Parsec m
  mempty = pure mempty

  {-# INLINE mempty #-}

type State :: UnliftedDatatype
data State = State {
    -- | the input string, in future this may be generalised
    input :: !String,
    -- | has the parser consumed input since the last relevant handler?
    consumed :: {-# UNPACK #-} !Word,
    -- | the current line number (incremented by \n)
    line :: {-# UNPACK #-} !Word,
    -- | the current column number (have to settle on a tab handling scheme)
    col  :: {-# UNPACK #-} !Word,
    -- | the valid for which hints can be used
    hintValidOffset :: {-# UNPACK #-} !Word,
    -- | the hints at this point in time
    hints :: !(Set ExpectItem)
  }

emptyState :: String -> State
emptyState !str = State { input = str
                        , consumed = 0
                        , line = 1
                        , col = 1
                        , hintValidOffset = 0
                        , hints = Set.empty
                        }

emptyErr :: State -> Word -> ParseError
emptyErr State{..} = Errors.emptyErr consumed line col

expectedErr :: State -> Set ExpectItem -> Word -> ParseError
expectedErr State{..} = Errors.expectedErr input consumed line col

specialisedErr :: State -> [String] -> CaretWidth -> ParseError
specialisedErr State{..} = Errors.specialisedErr consumed line col

unexpectedErr :: State -> Set ExpectItem -> String -> CaretWidth -> ParseError
unexpectedErr State{..} = Errors.unexpectedErr consumed line col

errorToHints :: State -> ParseError -> State
errorToHints st@State{..} err
  | consumed == Errors.presentationOffset err
  , not (Errors.isExpectedEmpty err) =
    if hintValidOffset < consumed then st { hints = Errors.expecteds err, hintValidOffset = consumed }
    else                               st { hints = Set.union hints (Errors.expecteds err) }
errorToHints st _ = st

useHints :: (ParseError -> State -> RT r) -> (ParseError -> State -> RT r)
useHints bad err st@State{hintValidOffset, hints}
  | presentationOffset == hintValidOffset = bad (Errors.useHints hints err) st
  | otherwise                             = bad err st{hintValidOffset = presentationOffset, hints = Set.empty}
  where !presentationOffset = Errors.presentationOffset err

adjustErr :: (ParseError -> ParseError) -> Parsec a -> Parsec a
adjustErr f (Parsec p) = Parsec $ \st good bad -> p st good $ \err -> bad (f err)
