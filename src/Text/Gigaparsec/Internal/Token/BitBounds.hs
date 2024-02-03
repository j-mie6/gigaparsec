{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, ConstraintKinds, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances, TypeFamilies, TypeOperators, CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.BitBounds (
    module Text.Gigaparsec.Internal.Token.BitBounds
  ) where

import Data.Kind (Constraint)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)

#if __GLASGOW_HASKELL__ >= 904

import GHC.TypeLits (type (<=?), Nat)
import GHC.TypeError (TypeError, ErrorMessage(Text, (:<>:), ShowType), Assert)

#else

import GHC.TypeLits (type (<=?), Nat, TypeError, ErrorMessage(Text, (:<>:), ShowType))

type Assert :: Bool -> Constraint -> Constraint
type family Assert b c where
  Assert 'True  _ = ()
  Assert 'False c = c

#endif

type Bits :: *
data Bits = B8 | B16 | B32 | B64

type BitWidth :: * -> Bits
type family BitWidth t where
  BitWidth Integer = 'B64
  BitWidth Int     = 'B64
  BitWidth Word    = 'B64
  BitWidth Word64  = 'B64
  BitWidth Natural = 'B64
  BitWidth Int32   = 'B32
  BitWidth Word32  = 'B32
  BitWidth Int16   = 'B16
  BitWidth Word16  = 'B16
  BitWidth Int8    = 'B8
  BitWidth Word8   = 'B8
  BitWidth t       = TypeError ('Text "The type '" ' :<>: 'ShowType t
                          ':<>: 'Text "' is not a numeric type supported by Gigaparsec")

type SignednessK :: *
data SignednessK = Signed | Unsigned

type Signedness :: * -> SignednessK -> Constraint
type family Signedness t s where
  Signedness Integer _         = () -- integers are allowed to serve as both unsigned and signed
  Signedness Int     'Signed   = ()
  Signedness Word    'Unsigned = ()
  Signedness Word64  'Unsigned = ()
  Signedness Natural 'Unsigned = ()
  Signedness Int32   'Signed   = ()
  Signedness Word32  'Unsigned = ()
  Signedness Int16   'Signed   = ()
  Signedness Word16  'Unsigned = ()
  Signedness Int8    'Signed   = ()
  Signedness Word8   'Unsigned = ()
  Signedness t       'Signed   = TypeError ('Text "The type '" ':<>: 'ShowType t
                                      ':<>: 'Text "' does not hold signed numbers")
  Signedness t       'Unsigned = TypeError ('Text "The type '" ' :<>: 'ShowType t
                                      ':<>: 'Text "' does not hold unsigned numbers")

type ShowBits :: Bits -> ErrorMessage
type ShowBits b = 'ShowType (BitsNat b)

-- This is intentionally not a type alias. On GHC versions < 9.4.1 it appears that TypeErrors are
-- reported slightly more eagerly and we get an error on this definition because
-- > BitsNat b <=? BitsNat (BitWidth t)
-- cannot be solved
type HasWidthFor :: Bits -> * -> Constraint
type family HasWidthFor bits t where
  HasWidthFor bits t = Assert (BitsNat bits <=? BitsNat (BitWidth t))
                              (TypeError ('Text "The type '"
                                    ':<>: 'ShowType t  ' :<>: 'Text "' cannot store a "
                                    ':<>: ShowBits bits ' :<>: 'Text " bit number (only supports up to "
                                    ':<>: ShowBits (BitWidth t) ' :<>: 'Text " bits)."))

type BitBounds :: Bits -> Constraint
class BitBounds b where
  upperSigned :: Integer
  lowerSigned :: Integer
  upperUnsigned :: Integer
  bits :: Bits
  type BitsNat b :: Nat
instance BitBounds 'B8 where
  upperSigned = fromIntegral (maxBound @Int8)
  lowerSigned = fromIntegral (minBound @Int8)
  upperUnsigned = fromIntegral (maxBound @Word8)
  bits = B8
  type BitsNat 'B8 = 8
instance BitBounds 'B16 where
  upperSigned = fromIntegral (maxBound @Int16)
  lowerSigned = fromIntegral (minBound @Int16)
  upperUnsigned = fromIntegral (maxBound @Word16)
  bits = B16
  type BitsNat 'B16 = 16
instance BitBounds 'B32 where
  upperSigned = fromIntegral (maxBound @Int32)
  lowerSigned = fromIntegral (minBound @Int32)
  upperUnsigned = fromIntegral (maxBound @Word32)
  bits = B32
  type BitsNat 'B32 = 32
instance BitBounds 'B64 where
  upperSigned = fromIntegral (maxBound @Int64)
  lowerSigned = fromIntegral (minBound @Int64)
  upperUnsigned = fromIntegral (maxBound @Word64)
  bits = B64
  type BitsNat 'B64 = 64

type CanHoldSigned :: Bits -> * -> Constraint
class (BitBounds bits, Num t) => CanHoldSigned bits t where
instance (BitBounds bits, Num t, Signedness t 'Signed, HasWidthFor bits t) => CanHoldSigned bits t

type CanHoldUnsigned :: Bits -> * -> Constraint
class (BitBounds bits, Num t) => CanHoldUnsigned bits t where
instance (BitBounds bits, Num t, Signedness t 'Unsigned, HasWidthFor bits t) => CanHoldUnsigned bits t
