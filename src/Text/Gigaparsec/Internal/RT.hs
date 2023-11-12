{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations, TupleSections, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.RT (module Text.Gigaparsec.Internal.RT) where

import GHC.Base (MutVar#, RealWorld, State#, runRW#, newMutVar#, readMutVar#, writeMutVar#)

import Control.Applicative (liftA, liftA2)
import Control.Monad (liftM2)
import Data.Coerce (coerce)
import GHC.IO (IO(IO))
import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))

type Reg :: * -> * -> *
type role Reg phantom representational
-- Don't even expose the constructor, then it's pretty much safe
data Reg r a = Reg (MutVar# RealWorld a)

type RT :: * -> *
newtype RT a = RT (State# RealWorld -> (# State# RealWorld, a #))

instance Functor RT where
  fmap :: (a -> b) -> RT a -> RT b
  fmap = liftA

  {-# INLINE fmap #-}

instance Applicative RT where
  pure :: a -> RT a
  pure x = RT (# , x #)

  liftA2 :: (a -> b -> c) -> RT a -> RT b -> RT c
  liftA2 = liftM2

  (*>) :: RT a -> RT b -> RT b
  RT m1 *> RT m2 = RT $ \s# ->
    case m1 s# of
      (# s'#, _ #) -> m2 s'#

  {-# INLINE pure #-}
  {-# INLINE liftA2 #-}
  {-# INLINE (*>) #-}

instance Monad RT where
  return :: a -> RT a
  return = pure

  (>>=) :: RT a -> (a -> RT b) -> RT b
  RT m >>= k = RT $ \s# ->
    case m s# of
      (# s'#, x #) -> let RT n = k x in n s'#

  (>>) :: RT a -> RT b -> RT b
  (>>) = (*>)

  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

newReg :: a -> (forall r. Reg r a -> RT b) -> RT b
newReg x k = RT $ \s# ->
  case newMutVar# x s# of
    (# s'#, reg# #) -> let RT k' = k (Reg reg#) in k' s'#

readReg :: Reg r a -> RT a
readReg (Reg reg#) = RT $ \s# -> readMutVar# reg# s#

writeReg :: Reg r a -> a -> RT ()
writeReg (Reg reg#) x = RT $ \s# ->
  case writeMutVar# reg# x s# of
    s'# -> (# s'#, () #)

ioToRT :: IO a -> RT a
ioToRT = coerce

rtToIO :: RT a -> IO a
rtToIO = coerce

fromIORef :: IORef a -> Reg r a
fromIORef (IORef (STRef reg#)) = Reg reg#
