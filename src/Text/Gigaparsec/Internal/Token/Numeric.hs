{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, ConstraintKinds, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, ApplicativeDo, TypeFamilies, TypeOperators, CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Text.Gigaparsec.Internal.Token.Numeric (module Text.Gigaparsec.Internal.Token.Numeric) where

import Text.Gigaparsec (Parsec, unit, void, atomic, (<|>), ($>))
import Text.Gigaparsec.Char (char, oneOf)
import Text.Gigaparsec.Combinator (optional, optionalAs)
import Text.Gigaparsec.Errors.Combinator (mapMaybeSWith)
import Text.Gigaparsec.Errors.ErrorGen (specializedGen, messages)
import Text.Gigaparsec.Token.Descriptions
    ( BreakCharDesc(BreakCharSupported, NoBreakChar),
      NumericDesc( NumericDesc, positiveSign, literalBreakChar
                 , integerNumbersCanBeHexadecimal, integerNumbersCanBeOctal
                 , integerNumbersCanBeBinary
                 , hexadecimalLeads, octalLeads, binaryLeads
                 ),
      PlusSignPresence(PlusIllegal, PlusRequired, PlusOptional) )
import Text.Gigaparsec.Internal.Token.Generic (GenericNumeric(plainDecimal, plainHexadecimal, plainOctal, plainBinary))
import Data.Char (intToDigit)
import Data.Kind (Constraint)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.Proxy (Proxy(Proxy))
import Control.Monad (when, unless)
import Numeric (showIntAtBase)

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
  bits :: Int
  type BitsNat b :: Nat
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
class (BitBounds bits, Num t) => CanHoldSigned bits t where
instance (BitBounds bits, Num t, Signedness t 'Signed, HasWidthFor bits t) => CanHoldSigned bits t

type CanHoldUnsigned :: Bits -> * -> Constraint
class (BitBounds bits, Num t) => CanHoldUnsigned bits t where
instance (BitBounds bits, Num t, Signedness t 'Unsigned, HasWidthFor bits t) => CanHoldUnsigned bits t

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

decimal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
decimal8 = decimalBounded @'B8
hexadecimal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
hexadecimal8 = hexadecimalBounded @'B8
octal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
octal8 = octalBounded @'B8
binary8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
binary8 = binaryBounded @'B8
number8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
number8 = numberBounded @'B8

decimal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
decimal16 = decimalBounded @'B16
hexadecimal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
hexadecimal16 = hexadecimalBounded @'B16
octal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
octal16 = octalBounded @'B16
binary16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
binary16 = binaryBounded @'B16
number16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
number16 = numberBounded @'B16

decimal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
decimal32 = decimalBounded @'B32
hexadecimal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
hexadecimal32 = hexadecimalBounded @'B32
octal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
octal32 = octalBounded @'B32
binary32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
binary32 = binaryBounded @'B32
number32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
number32 = numberBounded @'B32

decimal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
decimal64 = decimalBounded @'B64
hexadecimal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
hexadecimal64 = hexadecimalBounded @'B64
octal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
octal64 = octalBounded @'B64
binary64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
binary64 = binaryBounded @'B64
number64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
number64 = numberBounded @'B64

outOfBounds :: Integer -> Integer -> Int -> Integer -> [String]
outOfBounds small big radix _n = [
    "literal is not within the range " ++ resign small (" to " ++ resign big "")
  ]
  where resign n
          | n < 0 = ('-' :) . showIntAtBase (toInteger radix) intToDigit (abs n)
          | otherwise = showIntAtBase (toInteger radix) intToDigit n

mkUnsigned :: NumericDesc -> GenericNumeric -> IntegerParsers CanHoldUnsigned
mkUnsigned desc@NumericDesc{..} gen = IntegerParsers {..}
  where _bounded :: forall (bits :: Bits) t. CanHoldUnsigned bits t
                 => Proxy bits -> Parsec Integer -> Int -> Parsec t
        _bounded _ num radix = mapMaybeSWith
          (specializedGen { messages = outOfBounds 0 (upperUnsigned @bits) radix })
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
        _bounded _ num radix = mapMaybeSWith
          (specializedGen { messages = outOfBounds (lowerSigned @bits) (upperSigned @bits) radix })
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
