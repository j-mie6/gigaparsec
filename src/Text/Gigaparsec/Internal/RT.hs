{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations, UnboxedTuples, DerivingVia #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.RT (module Text.Gigaparsec.Internal.RT) where

import GHC.Base (MutVar#, RealWorld, State#, runRW#, newMutVar#, readMutVar#, writeMutVar#)

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
  deriving (Functor, Applicative, Monad) via IO

{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

{-# INLINABLE newReg #-}
newReg :: a -> (forall r. Reg r a -> RT b) -> RT b
newReg x k = RT $ \s# ->
  case newMutVar# x s# of
    (# s'#, reg# #) -> let RT k' = k (Reg reg#) in k' s'#

{-# INLINE readReg #-}
readReg :: Reg r a -> RT a
readReg (Reg reg#) = RT $ \s# -> readMutVar# reg# s#

{-# INLINABLE writeReg #-}
writeReg :: Reg r a -> a -> RT ()
writeReg (Reg reg#) x = RT $ \s# ->
  case writeMutVar# reg# x s# of
    s'# -> (# s'#, () #)

{-# INLINE unsafeIOToRT #-}
unsafeIOToRT :: IO a -> RT a
unsafeIOToRT = coerce

{-# INLINE rtToIO #-}
rtToIO :: RT a -> IO a
rtToIO = coerce

{-# INLINE fromIORef #-}
fromIORef :: IORef a -> Reg r a
fromIORef (IORef (STRef reg#)) = Reg reg#
