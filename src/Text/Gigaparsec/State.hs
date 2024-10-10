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
    The Ref type describes pieces of state that are threaded through a parser. 
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

{-|
Run a parser @f@ parameterised by a reference, which is uninitialised.

The function @f@ effectively defines a parser that has access to a new uninitialised reference of type @a@.
This reference __must__ be initialised by @f@ before it is read from, otherwise an 'error' will be thrown.
The parameterization of 'Ref' by @r@ ensures that the reference cannot escape the scope of the inner parser defined by @f@.

This function is __unsafe__, meaning it will throw a GHC `error` if the reference is read before it is set.
-}
unsafeMake :: (forall r. Ref r a -> Parsec b) -> Parsec b
unsafeMake = make (error "reference used but not set")

_make :: Parsec a -> (forall r. Ref r a -> Parsec b) -> Parsec b
_make p f = p >>= \x -> make x f

{-|
Run a parser @f@ parameterised by a reference with initial value @x@.

The function @f@ effectively defines a parser that has access to a new reference of type @a@, whose initial value is @x@.
The parameterization of 'Ref' by @r@ ensures that the reference cannot escape the scope of the inner parser defined by @f@.
-}
make  :: a                               -- ^ @x@, the initial value the reference will hold.
      -> (forall r. Ref r a -> Parsec b) -- ^ @f@, a function which runs a parser with access to the given reference 
      -> Parsec b                        -- ^ a parser that produces a @b@ with access to the reference
make x f = Internal.Parsec $ \st good bad ->
  newRef x $ \ref ->
    let Internal.Parsec p = f ref
    in p st good bad

{-|
Return the current value of a reference.

Given a reference @ref@, this produces a parser that simply returns the current value of @ref@.

This parser consumes no input and always succeeds.
-}
get :: Ref r a -> Parsec a
get ref = Internal.Parsec $ \st good _ ->
  do x <- readRef ref
     good x st

-- parsley provides multiple overloadings...
_gets :: Ref r a -> Parsec (a -> b) -> Parsec b
_gets ref pf = pf <*> get ref

{-|
Get a specific component of the reference, using a projection function supplied.

Given a reference @ref@, and a function @f@, this function produces a parser which returns the
value of applying @f@ to the current value of @ref@.

This parser consumes no input and always succeeds.
-}
gets  :: Ref r a  -- ^ @ref@, The reference to extract a value from
      -> (a -> b) -- ^ @f@, The function which processes the current value of @ref@.
      -> Parsec b -- ^ a parser returning the result of @f@ applied to the current value of @ref@.
gets ref f = f <$> get ref

_set :: Ref r a -> Parsec a -> Parsec ()
_set ref px = px >>= set ref

{-|
Replace the value of the given reference.

Given a reference @ref@, and a value @x@, this function replaces the current value of @ref@ with @x@.
This produces a parser which changes the value of @ref@ during any subsequent parsing.

This parser consumes no input and always succeeds.
-}
set :: Ref r a -> a -> Parsec ()
set ref x = Internal.Parsec $ \st good _ ->
  do writeRef ref x
     good () st

{-|
Replace the value of the reference by the result of applying a function to the result of the given parser.

Given a parser @p@, a function @f@, and reference @ref@: 
run the parser @p@, which produces a result of type @a@;
then replace the value of @ref@ by that of @f@ applied to the result of @p@.
This produces a parser which changes the value of @ref@ during any subsequent parsing.

This parser consumes input and fails if and only if the given parser @p@ does also.
-}
sets  :: Ref r b   -- ^ @ref@, the reference whose value we wish to replace.
      -> (a -> b)  -- ^ @f@, the function to apply to the result of @p@.
      -> Parsec a  -- ^ @p@, the given parser.
      -> Parsec () -- ^ a parser which runs @p@, and replaces the value of @ref@ to be that of applying @f@ to the result of @p@.
sets ref f px = _set ref (f <$> px)

_update :: Ref r a -> Parsec (a -> a) -> Parsec ()
_update ref pf = _set ref (_gets ref pf)

{-|
Set the new value of the reference to be the result of applying a function to the old value.

Given a function @f@ and reference @ref@; the current value of @ref@ is replaced by that of applying @f@ to this value.

This parser consumes no input and always succeeds.
-}
update :: Ref r a -> (a -> a) -> Parsec ()
update ref f = _set ref (gets ref f)

{-|
Run the given parser @p@ with a modified value of the reference, and then reset this value if @p@ succeeds.

Behaves like 'update', except the scope of the update of the reference @ref@ is limited just to the given parser @p@, 
assuming that @p@ succeeds.
If the parser @p@ fails, then the value of the reference @ref@ is __not reset__ to its original value.
In summary, this parser unconditionally modifies the value of @ref@, and resets the value of @ref@ only when @p@ succeeds.

This parser consumes input and fails if and only if the given parser @p@ does also.
-}
updateDuring  :: Ref r a  -- ^ @ref@, the reference to modify.
              -> (a -> a) -- ^ @xf, the function to modify the value of @ref@. 
              -> Parsec b -- ^ @p@, the parser to run with the modified reference.
              -> Parsec b -- ^ a parser which runs @p@ with the modified reference, and resets @ref@ if @p@ succeeds.
updateDuring ref f p = do x <- get ref
                          set ref (f x)
                          p <* set ref x

{-|
Run the given parser @p@ with a new value of the reference, and then reset this value if @p@ succeeds.

Behaves like 'updateDuring', except the value of the reference is simply replaced by the given value @x@.
If the parser @p@ fails, then the value of the reference @ref@ is __not reset__ to its original value.

This parser consumes input and fails if and only if the given parser @p@ does also.
-}
setDuring :: Ref r a  -- ^ @ref@, the reference to modify.
          -> a        -- ^ @x@, the value to temporarily replace that of @ref@.
          -> Parsec b -- ^ @p@, the parser to run with the modified reference.
          -> Parsec b -- ^ a parser which runs @p@ with the modified reference, and resets @ref@ if @p@ succeeds.
setDuring ref x = updateDuring ref (const x)

_setDuring :: Ref r a -> Parsec a -> Parsec b -> Parsec b
_setDuring ref px q = px >>= flip (setDuring ref) q

{-|
Run a parser, and if it fails __without consuming input__, undo its modifications to the given reference.

This parser consumes input only if @p@ does also; it fails if and only if @p@ fails __having consumed input__.
-}
rollback  :: Ref r a  -- ^ @ref@, the reference whose value will be 'rolled-back' on failure of @p@
          -> Parsec b -- ^ @p@, the parser to run
          -> Parsec b -- ^ a parser that runs @p@, and restores the original value of @ref@ if @p@ fails without consuming input.
rollback ref p = get ref >>= \x -> p <|> (set ref x *> empty)

{-|
Repeatedly execute a parser in a loop until the condition passes.

@'forP' ini cond step body@ behaves much like a traditional for loop using @ini@, @cond@, @step@,
and @body@ as parsers which control the loop itself. 
In pseudocode, this would be equivalent to:

@
  results = []
  for (i: a := ini ; not (cond i) ; i := step i ) {
      r <- body
      results.append r
  }
  return results
@

-}
forP  :: Parsec a           -- ^ @ini@,  the initial value of the iterator.
      -> Parsec (a -> Bool) -- ^ @cond@, the condition by which the loop terminates.
      -> Parsec (a -> a)    -- ^ @step@, how the iterator is updated on each iteration.
      -> Parsec b           -- ^ @body@, the parser to run on each iteration.
      -> Parsec [b]         -- ^ a parser that repeatedly parses @body@ until @cond@ is satisfied.
forP ini cond step = forP' ini cond step . const

{-|
Repeatedly execute a parser in a loop until the condition passes.

'forP'' is similar to 'forP', except the @body@ of the loop is able to access the value of the iterator.
In pseudocode, this would be equivalent to:

@
  results = []
  for (i: a := ini ; not (cond i) ; i := step i ) {
      r <- body i
      results.append r
  }
  return results
@

-}
forP' :: Parsec a           -- ^ @ini@,  the initial value of the iterator.
      -> Parsec (a -> Bool) -- ^ @cond@, the condition by which the loop terminates.
      -> Parsec (a -> a)    -- ^ @step@, how the iterator is updated on each iteration.
      -> (a -> Parsec b)    -- ^ @body@, the parser to run on each iteration, parameterised by the current iterator value.
      -> Parsec [b]         -- ^ a parser that repeatedly parses @body@ until @cond@ is satisfied.
forP' ini cond step body = ini >>= go
  where go i = flip (ifS (cond <*> pure i)) (pure []) do
                  x <- body i
                  f <- step
                  xs <- go (f i)
                  return (x : xs)

{-|
Repeatedly execute a parser in a loop until the condition passes, ignoring any results.

@'forP_' ini cond step body@ behaves much like a traditional for loop using @ini@, @cond@, @step@,
and @body@ as parsers which control the loop itself. 
Unlike 'forP', this function ignores any results from the @body@ parser.
-}
forP_ :: Parsec a           -- ^ @ini@,  the initial value of the iterator.
      -> Parsec (a -> Bool) -- ^ @cond@, the condition by which the loop terminates.
      -> Parsec (a -> a)    -- ^ @step@, how the iterator is updated on each iteration.
      -> Parsec b           -- ^ @body@, the parser to run on each iteration.
      -> Parsec ()          -- ^ a parser that repeatedly parses @body@ until @cond@ is satisfied, ignoring any results from @body@.
forP_ ini cond step = forP'_ ini cond step . const

{-|
Repeatedly execute a parser in a loop until the condition passes, ignoring any results.

'forP'_' is similar to 'forP_', except the @body@ of the loop is able to access the value of the iterator.
-}
forP'_  :: Parsec a           -- ^ @ini@,  the initial value of the iterator.
        -> Parsec (a -> Bool) -- ^ @cond@, the condition by which the loop terminates.
        -> Parsec (a -> a)    -- ^ @step@, how the iterator is updated on each iteration.
        -> (a -> Parsec b)    -- ^ @body@, the parser to run on each iteration, parameterised by the current iterator value.
        -> Parsec ()          -- ^ a parser that repeatedly parses @body@ until @cond@ is satisfied, ignoring any results from @body@.
forP'_ ini cond step body = ini >>= go
  where go i = whenS (cond <*> pure i) do
                  body i
                  f <- step
                  go (f i)
