{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, NamedFieldPuns, CPP #-}
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
import Text.Gigaparsec.Internal.Errors (Error, Hints, ExpectItem, CaretWidth)
import Text.Gigaparsec.Internal.Errors qualified as Errors (
    emptyErr, expectedErr, specialisedErr, mergeErr, unexpectedErr,
    isExpectedEmpty, presentationOffset, useHints, DefuncHints(Blank), addError
  )

import Control.Applicative (Applicative(liftA2), Alternative(empty, (<|>), many, some)) -- liftA2 required until 9.6
import Control.Selective (Selective(select))

import Data.Set (Set)

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

{-|
This type represents parsers as a first-class value.

Values of this type are constructed using the library's combinators, to build
up a final 'Parsec' value that can be passed to 'Text.Gigaparsec.parse' or one
of the similar functions. This is implemented internally similar to other
libraries like @parsec@ and @gigaparsec@.
-}
type Parsec :: * -> *
newtype Parsec a = Parsec {
    unParsec :: forall r. State
             -> (a -> State -> RT r)     -- the good continuation
             -> (Error -> State -> RT r) -- the bad continuation
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

raise :: (State -> Error) -> Parsec a
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

{-|
This combinator will parse this parser __zero__ or more times combining the results with the function @f@ and base value @k@ from the right.

This parser will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a right-to-left
fashion using the function @f@: the right-most value provided to @f@ is the value @k@.
If this parser does fail at any point having consumed input, this combinator will fail.

==== __Examples__
> many = manyr (:) []
-}
{-# INLINE manyr #-}
manyr :: (a -> b -> b) -- ^ function to apply to each value produced by this parser, starting at the right.
      -> b             -- ^ value to use when this parser no longer succeeds.
      -> Parsec a
      -> Parsec b      -- ^ a parser which parses this parser many times and folds the results together with @f@ and @k@ right-associatively.
manyr f k p = let go = liftA2 f p go <|> pure k in go

{-|
This combinator will parse this parser __one__ or more times combining the results with the function @f@ and base value @k@ from the right.

This parser will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a right-to-left
fashion using the function @f@: the right-most value provided to @f@ is the value @k@.
If this parser does fail at any point having consumed input, this combinator will fail.

==== __Examples__
> some = somer (:) []
-}
{-# INLINE somer #-}
somer :: (a -> b -> b) -- ^ function to apply to each value produced by this parser, starting at the right.
      -> b             -- ^ value to use when this parser no longer succeeds.
      -> Parsec a
      -> Parsec b      -- ^ a parser which parses this parser some times and folds the results together with @f@ and @k@ right-associatively.
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
    hintsValidOffset :: {-# UNPACK #-} !Word,
    -- | the hints at this point in time
    hints :: Hints,
    -- | Debug nesting
    debugLevel :: {-# UNPACK #-} !Int
  }

emptyState :: String -> State
emptyState !str = State { input = str
                        , consumed = 0
                        , line = 1
                        , col = 1
                        , hintsValidOffset = 0
                        , hints = Errors.Blank
                        , debugLevel = 0
                        }

emptyErr :: State -> Word -> Error
emptyErr State{..} = Errors.emptyErr consumed line col

expectedErr :: State -> Set ExpectItem -> Word -> Error
expectedErr State{..} = Errors.expectedErr input consumed line col

specialisedErr :: State -> [String] -> CaretWidth -> Error
specialisedErr State{..} = Errors.specialisedErr consumed line col

unexpectedErr :: State -> Set ExpectItem -> String -> CaretWidth -> Error
unexpectedErr State{..} = Errors.unexpectedErr consumed line col

errorToHints :: State -> Error -> State
errorToHints st@State{..} err
  | consumed == Errors.presentationOffset err
  , not (Errors.isExpectedEmpty err) =
    if hintsValidOffset < consumed then st { hints = Errors.addError (Errors.Blank) err, hintsValidOffset = consumed }
    else                                st { hints = Errors.addError hints err }
errorToHints st _ = st

useHints :: (Error -> State -> RT r) -> (Error -> State -> RT r)
useHints bad err st@State{hintsValidOffset, hints}
  | presentationOffset == hintsValidOffset = bad (Errors.useHints hints err) st
  | otherwise                              = bad err st{ hintsValidOffset = presentationOffset, hints = Errors.Blank }
  where !presentationOffset = Errors.presentationOffset err

adjustErr :: (Error -> Error) -> Parsec a -> Parsec a
adjustErr f (Parsec p) = Parsec $ \st good bad -> p st good $ \err -> bad (f err)
