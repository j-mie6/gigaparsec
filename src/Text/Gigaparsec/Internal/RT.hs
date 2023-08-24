{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations, TupleSections, UnboxedTuples #-}
module Text.Gigaparsec.Internal.RT (module Text.Gigaparsec.Internal.RT) where

import GHC.Base (MutVar#, RealWorld, State#, runRW#, newMutVar#, readMutVar#, writeMutVar#)

import Control.Applicative (liftA, liftA2)
import Control.Monad (liftM2)

type Reg :: * -> * -> *
type role Reg phantom representational
data Reg r a = Reg (MutVar# RealWorld a)

type RT :: * -> *
newtype RT a = RT (State# RealWorld -> (# State# RealWorld, a #))

instance Functor RT where
  fmap :: (a -> b) -> RT a -> RT b
  fmap = liftA -- TODO:

  {-# INLINE fmap #-}

-- TODO: (*>), (<*), (<*>)?
instance Applicative RT where
  pure :: a -> RT a
  pure x = RT (# , x #)

  liftA2 :: (a -> b -> c) -> RT a -> RT b -> RT c
  liftA2 = liftM2 -- TODO:

  {-# INLINE pure #-}
  {-# INLINE liftA2 #-}

-- TODO: (>>)
instance Monad RT where
  return :: a -> RT a
  return = pure

  (>>=) :: RT a -> (a -> RT b) -> RT b
  RT m >>= k = RT $ \s# ->
    case m s# of
      (# s'#, x #) -> let RT n = k x in n s'#

  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

newReg :: a -> RT (Reg r a)
newReg x = RT $ \s# ->
  case newMutVar# x s# of
    (# s'#, reg# #) -> (# s'#, Reg reg# #)

readReg :: Reg r a -> RT a
readReg (Reg reg#) = RT $ \s# -> readMutVar# reg# s#

writeReg :: Reg r a -> a -> RT ()
writeReg (Reg reg#) x = RT $ \s# ->
  case writeMutVar# reg# x s# of
    s'# -> (# s'#, () #)
