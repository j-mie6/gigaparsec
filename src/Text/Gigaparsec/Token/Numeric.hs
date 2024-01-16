{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, ApplicativeDo, TypeFamilies, 
TypeOperators, CPP #-}
#include "portable-unlifted.h"
-- TODO: refine, move to Internal
module Text.Gigaparsec.Token.Numeric (module Text.Gigaparsec.Token.Numeric) where

import Text.Gigaparsec (Parsec, mapMaybeS, unit, void, atomic, (<|>), ($>))
import Text.Gigaparsec.Char (char, oneOf)
import Text.Gigaparsec.Combinator (optional, optionalAs)
import Text.Gigaparsec.Token.Descriptions
    ( BreakCharDesc(BreakCharSupported, NoBreakChar),
      NumericDesc( NumericDesc, positiveSign, literalBreakChar
                 , integerNumbersCanBeHexadecimal, integerNumbersCanBeOctal
                 , integerNumbersCanBeBinary
                 , hexadecimalLeads, octalLeads, binaryLeads
                 ),
      PlusSignPresence(PlusIllegal, PlusRequired, PlusOptional) )
import Text.Gigaparsec.Token.Generic (GenericNumeric(plainDecimal, plainHexadecimal, plainOctal, plainBinary))
import Data.Kind (Constraint)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.Proxy (Proxy(Proxy))
import Control.Monad (when, unless)

#if __GLASGOW_HASKELL__ >= 904

import GHC.TypeLits (type (<=?))
import GHC.TypeError(TypeError, ErrorMessage(Text, (:<>:), ShowType), Assert)

#else

import GHC.TypeLits (type (<=))
import GHC.TypeLits(TypeError, ErrorMessage(Text, (:<>:), ShowType))

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
  BitWidth a       
        = TypeError ('Text "The type '" ' :<>: 'ShowType a 
   ' :<>: 'Text "' is not a numeric type supported by Gigaparsec")

type Signedness :: *
data Signedness = Signed | Unsigned

type IsSigned :: * -> Signedness -> Constraint
type family IsSigned t s where
  IsSigned Integer 'Signed   = ()
  IsSigned Int     'Signed   = ()
  IsSigned Word    'Unsigned = ()
  IsSigned Word64  'Unsigned = ()
  IsSigned Natural 'Unsigned = ()
  IsSigned Int32   'Signed   = ()
  IsSigned Word32  'Unsigned = ()
  IsSigned Int16   'Signed   = ()
  IsSigned Word16  'Unsigned = ()
  IsSigned Int8    'Signed   = ()
  IsSigned Word8   'Unsigned = ()
  IsSigned a       'Signed
      = TypeError ('Text "The type '" ' :<>: 'ShowType a ' :<>: 'Text "' does not hold unsigned data")
  IsSigned a       'Unsigned     
      = TypeError ('Text "The type '" ' :<>: 'ShowType a ' :<>: 'Text "' does not hold signed data")

type ShowBits :: Bits -> ErrorMessage
type ShowBits b = 'ShowType (BitsNat b)

type SatisfiesBound :: * -> Bits -> Constraint
type SatisfiesBound t b 

#if __GLASGOW_HASKELL__ >= 904

      = Assert (BitsNat b <=? BitsNat (BitWidth t)) (TypeError ('Text "The type '" 
 ' :<>: 'ShowType t  ' :<>: 'Text "' does not have enough bit-width to store " 
 ' :<>: ShowBits (BitWidth t) ' :<>: 'Text " bits of data (can only store up to" ' :<>: ShowBits b 
 ' :<>: 'Text " bits)."))

#else

  = BitsNat b <= BitsNat (BitWidth t)

#endif

type BitBounds :: Bits -> Constraint
class BitBounds b where
  upperSigned :: Integer
  lowerSigned :: Integer
  upperUnsigned :: Integer
  bits :: Int
  type BitsNat b :: Natural 
instance BitBounds 'B8 where
  upperSigned = fromIntegral (maxBound @Int8)
  lowerSigned = fromIntegral (minBound @Int8)
  upperUnsigned = fromIntegral (maxBound @Word8)
  bits = 8
  type BitsNat 'B8 = 8
instance BitBounds 'B16 where
  upperSigned = fromIntegral (maxBound @Int16)
  lowerSigned = fromIntegral (minBound @Int16)
  upperUnsigned = fromIntegral (maxBound @Word16)
  bits = 16
  type BitsNat 'B16 = 16
instance BitBounds 'B32 where
  upperSigned = fromIntegral (maxBound @Int32)
  lowerSigned = fromIntegral (minBound @Int32)
  upperUnsigned = fromIntegral (maxBound @Word32)
  bits = 32
  type BitsNat 'B32 = 32
instance BitBounds 'B64 where
  upperSigned = fromIntegral (maxBound @Int64)
  lowerSigned = fromIntegral (minBound @Int64)
  upperUnsigned = fromIntegral (maxBound @Word64)
  bits = 64
  type BitsNat 'B64 = 64

type CanHoldSigned :: Bits -> * -> Constraint
class (BitBounds b, Num a) => CanHoldSigned b a where
instance (BitBounds b, Num a, IsSigned a 'Signed, SatisfiesBound a b) => CanHoldSigned b a

type CanHoldUnsigned :: Bits -> * -> Constraint
class (BitBounds b, Num a) => CanHoldUnsigned b a where
instance (BitBounds b, Num a, IsSigned a 'Unsigned, SatisfiesBound a b) => CanHoldUnsigned b a

type IntegerParsers :: (Bits -> * -> Constraint) -> *
data IntegerParsers canHold = IntegerParsers { decimal :: Parsec Integer
                                             , hexadecimal :: Parsec Integer
                                             , octal :: Parsec Integer
                                             , binary :: Parsec Integer
                                             , number :: Parsec Integer
                                             , _bounded :: forall (bits :: Bits) t. canHold bits t => Proxy bits -> Parsec Integer -> Int -> Parsec t
                                             }

decimalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
decimalBounded IntegerParsers{..} = _bounded (Proxy @bits) decimal 10

hexadecimalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
hexadecimalBounded IntegerParsers{..} = _bounded (Proxy @bits) hexadecimal 16

octalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
octalBounded IntegerParsers{..} = _bounded (Proxy @bits) octal 8

binaryBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
binaryBounded IntegerParsers{..} = _bounded (Proxy @bits) binary 2

numberBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
numberBounded IntegerParsers{..} = _bounded (Proxy @bits) number 10

decimal8 :: canHold 'B8 a => IntegerParsers canHold -> Parsec a
decimal8 = decimalBounded @'B8
hexadecimal8 :: canHold 'B8 a => IntegerParsers canHold -> Parsec a
hexadecimal8 = hexadecimalBounded @'B8
octal8 :: canHold 'B8 a => IntegerParsers canHold -> Parsec a
octal8 = octalBounded @'B8
binary8 :: canHold 'B8 a => IntegerParsers canHold -> Parsec a
binary8 = binaryBounded @'B8
number8 :: canHold 'B8 a => IntegerParsers canHold -> Parsec a
number8 = numberBounded @'B8

decimal16 :: canHold 'B16 a => IntegerParsers canHold -> Parsec a
decimal16 = decimalBounded @'B16
hexadecimal16 :: canHold 'B16 a => IntegerParsers canHold -> Parsec a
hexadecimal16 = hexadecimalBounded @'B16
octal16 :: canHold 'B16 a => IntegerParsers canHold -> Parsec a
octal16 = octalBounded @'B16
binary16 :: canHold 'B16 a => IntegerParsers canHold -> Parsec a
binary16 = binaryBounded @'B16
number16 :: canHold 'B16 a => IntegerParsers canHold -> Parsec a
number16 = numberBounded @'B16

decimal32 :: canHold 'B32 a => IntegerParsers canHold -> Parsec a
decimal32 = decimalBounded @'B32
hexadecimal32 :: canHold 'B32 a => IntegerParsers canHold -> Parsec a
hexadecimal32 = hexadecimalBounded @'B32
octal32 :: canHold 'B32 a => IntegerParsers canHold -> Parsec a
octal32 = octalBounded @'B32
binary32 :: canHold 'B32 a => IntegerParsers canHold -> Parsec a
binary32 = binaryBounded @'B32
number32 :: canHold 'B32 a => IntegerParsers canHold -> Parsec a
number32 = numberBounded @'B32

decimal64 :: canHold 'B64 a => IntegerParsers canHold -> Parsec a
decimal64 = decimalBounded @'B64
hexadecimal64 :: canHold 'B64 a => IntegerParsers canHold -> Parsec a
hexadecimal64 = hexadecimalBounded @'B64
octal64 :: canHold 'B64 a => IntegerParsers canHold -> Parsec a
octal64 = octalBounded @'B64
binary64 :: canHold 'B64 a => IntegerParsers canHold -> Parsec a
binary64 = binaryBounded @'B64
number64 :: canHold 'B64 a => IntegerParsers canHold -> Parsec a
number64 = numberBounded @'B64

mkUnsigned :: NumericDesc -> GenericNumeric -> IntegerParsers CanHoldUnsigned
mkUnsigned desc@NumericDesc{..} gen = IntegerParsers {..}
  where _bounded :: forall (bits :: Bits) t. CanHoldUnsigned bits t
                 => Proxy bits -> Parsec Integer -> Int -> Parsec t
        _bounded _ num _radix = mapMaybeS
          (\n -> if n >= 0 && n <= upperUnsigned @bits then Just (fromInteger n) else Nothing)
          num

        leadingBreakChar = case literalBreakChar of
          NoBreakChar -> unit
          BreakCharSupported breakChar allowedAfterNonDecimalPrefix ->
            when allowedAfterNonDecimalPrefix (optional (char breakChar))

        noZeroHexadecimal = do
          unless (null hexadecimalLeads) (void (oneOf hexadecimalLeads))
          leadingBreakChar
          plainHexadecimal gen desc

        noZeroOctal = do
          unless (null octalLeads) (void (oneOf octalLeads))
          leadingBreakChar
          plainOctal gen desc

        noZeroBinary = do
          unless (null binaryLeads) (void (oneOf binaryLeads))
          leadingBreakChar
          plainBinary gen desc

        decimal = plainDecimal gen desc
        hexadecimal = atomic (char '0' *> noZeroHexadecimal)
        octal = atomic (char '0' *> noZeroOctal)
        binary = atomic (char '0' *> noZeroBinary)
        number
          | not integerNumbersCanBeBinary
          , not integerNumbersCanBeHexadecimal
          , not integerNumbersCanBeOctal = decimal
          | otherwise = atomic (zeroLead <|> decimal)
          where zeroLead = char '0' *> addHex (addOct (addBin (decimal <|> pure 0)))
                addHex
                  | integerNumbersCanBeHexadecimal = (noZeroHexadecimal <|>)
                  | otherwise = id
                addOct
                  | integerNumbersCanBeOctal = (noZeroOctal <|>)
                  | otherwise = id
                addBin
                  | integerNumbersCanBeBinary = (noZeroBinary <|>)
                  | otherwise = id

mkSigned :: NumericDesc -> IntegerParsers c -> IntegerParsers CanHoldSigned
mkSigned NumericDesc{..} unsigned = IntegerParsers {
    decimal = _decimal,
    hexadecimal = _hexadecimal,
    octal = _octal,
    binary = _binary,
    number = _number,
    ..
  }
  where _bounded :: forall (bits :: Bits) t. CanHoldSigned bits t
                 => Proxy bits -> Parsec Integer -> Int -> Parsec t
        _bounded _ num _radix = mapMaybeS
          (\n -> if n >= lowerSigned @bits && n <= upperSigned @bits
                 then Just (fromInteger n)
                 else Nothing)
          num

        sign :: Parsec (Integer -> Integer)
        sign = case positiveSign of
          PlusRequired -> char '+' $> id <|> char '-' $> negate
          PlusOptional -> char '-' $> negate <|> optionalAs id (char '+')
          PlusIllegal  -> pure id
        _decimal = atomic (sign <*> decimal unsigned)
        _hexadecimal = atomic (sign <*> hexadecimal unsigned)
        _octal = atomic (sign <*> octal unsigned)
        _binary = atomic (sign <*> binary unsigned)
        _number = atomic (sign <*> number unsigned)

{-type FloatingParsers :: *
data FloatingParsers = FloatingParsers {}

mkUnsignedFloating :: NumericDesc -> IntegerParsers CanHoldUnsigned -> GenericNumeric -> FloatingParsers
mkUnsignedFloating NumericDesc{..} nat gen = FloatingParsers {}

mkSignedFloating :: NumericDesc -> FloatingParsers -> FloatingParsers
mkSignedFloating NumericDesc{..} unsigned = FloatingParsers {}

type CombinedParsers :: *
data CombinedParsers = CombinedParsers {}

mkUnsignedCombined :: NumericDesc -> IntegerParsers CanHoldUnsigned -> FloatingParsers -> CombinedParsers
mkUnsignedCombined NumericDesc{..} natural floating = CombinedParsers {}

mkSignedCombined :: NumericDesc -> CombinedParsers -> CombinedParsers
mkSignedCombined NumericDesc{..} unsigned = CombinedParsers {}-}

lexemeInteger :: (forall a. Parsec a -> Parsec a) -> IntegerParsers c -> IntegerParsers c
lexemeInteger lexe IntegerParsers{..} = IntegerParsers {
    decimal = lexe decimal,
    hexadecimal = lexe hexadecimal,
    octal = lexe octal,
    binary = lexe binary,
    number = lexe number,
    _bounded = \n b radix -> lexe (_bounded n b radix)
  }

{-lexemeFloating :: (forall a. Parsec a -> Parsec a) -> FloatingParsers -> FloatingParsers
lexemeFloating = const id

lexemeCombined :: (forall a. Parsec a -> Parsec a) -> CombinedParsers -> CombinedParsers
lexemeCombined = const id
-}
