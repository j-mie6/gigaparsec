{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, TypeOperators, FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
module Text.Gigaparsec.Expr.Subtype (module Text.Gigaparsec.Expr.Subtype) where

import Data.Kind (Constraint)
--import Control.Monad ((>=>))

type Subtype :: * -> * -> Constraint
class Subtype sub sup where
  upcast :: sub -> sup
  downcast :: sup -> Maybe sub

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
