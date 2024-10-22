{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, TypeOperators, FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
{-|
Module      : Text.Gigaparsec.Expr.Subtype
Description : This module defines explicit subtyping, with up- and -downcasting.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module defines explicit subtyping, with up- and -downcasting.

Subtyping is used in 'Text.Gigaparsec.Expr' to allow for more specific types within a single layer of a precedence table,
as long as they all have a common supertype.
-}
module Text.Gigaparsec.Expr.Subtype (
  -- ** Subtyping
  {-|
  -}
  Subtype(upcast, downcast),
  type (<),
  -- ** Module Re-export
  -- | This should be removed.
  module Text.Gigaparsec.Expr.Subtype) where

import Data.Kind (Constraint)
--import Control.Monad ((>=>))

{-|
Explicit subtyping with up- and down-casting.

@sub@ is a 'Subtype' of @sup@ when there is an /upcasting/ function @sub -> sup@.
The intuition is that for each value of type @sub@, there is a (unique) corresponding value in @sup@
`upcast` will convert values in @sub@ to their corresponding values in @sup@.

The `downcast` function describes all those values in @sup@ which correspond to values in @sub@;
if @v :: sup@ does not correspond to any value in @sub@, then @'downcast' v = 'Nothing'@.

This is encapsulated by the following property: 

prop> downcast . upcast = Just

meaning that `upcast` should be a right inverse for `downcast`.
In other words, if you `upcast` and then `downcast` some value, you will end up with the same value 
(albeit wrapped under a 'Just').

-}
type Subtype :: * -> * -> Constraint
class Subtype sub sup where
  {-| 
  Inject the subtype into the supertype.

  Cast values in @sub@ into a value of type @sup@.
  This should be a right inverse of 'downcast'.
  -}
  upcast :: sub -> sup
  {-|
  Describes which elements of @sup@ correspond with those in @sub@.
  
  That is, @'downcast' v = 'Just' w@  precisely when  @'upcast' w = v@.

  If @v :: sup@ corresponds with some element @w :: sub@, then,

  > downcast v = Just w
  
  Otherwise, if @v :: sup@ is not the upcast of any element of @sub@, then,

  > downcast v = Nothing
  
  -}
  downcast :: sup -> Maybe sub

{-| 
An infix alias of 'Subtype'.
-}
type (<) :: * -> * -> Constraint
type sub < sup = Subtype sub sup

instance Subtype a a where
  upcast = id
  downcast = Just

-- This instance is just evil
{-instance forall b a c. (a < b, b < c) => Subtype a c where
  upcast = upcast @b @c . upcast @a @b
  downcast = downcast @b @c >=> downcast @a @b
-}
