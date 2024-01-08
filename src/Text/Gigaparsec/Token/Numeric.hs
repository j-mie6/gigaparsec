{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, ApplicativeDo #-}
-- TODO: refine
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

type Bits :: *
data Bits = B8 | B16 | B32 | B64

type BitBounds :: Bits -> Constraint
class BitBounds b where
  upperSigned :: Integer
  lowerSigned :: Integer
  upperUnsigned :: Integer
  bits :: Int
instance BitBounds 'B8 where
  upperSigned = fromIntegral (maxBound @Int8)
  lowerSigned = fromIntegral (minBound @Int8)
  upperUnsigned = fromIntegral (maxBound @Word8)
  bits = 8
instance BitBounds 'B16 where
  upperSigned = fromIntegral (maxBound @Int16)
  lowerSigned = fromIntegral (minBound @Int16)
  upperUnsigned = fromIntegral (maxBound @Word16)
  bits = 16
instance BitBounds 'B32 where
  upperSigned = fromIntegral (maxBound @Int32)
  lowerSigned = fromIntegral (minBound @Int32)
  upperUnsigned = fromIntegral (maxBound @Word32)
  bits = 32
instance BitBounds 'B64 where
  upperSigned = fromIntegral (maxBound @Int64)
  lowerSigned = fromIntegral (minBound @Int64)
  upperUnsigned = fromIntegral (maxBound @Word64)
  bits = 64

type CanHoldSigned :: Bits -> * -> Constraint
class (BitBounds b, Num a) => CanHoldSigned b a where
instance CanHoldSigned 'B8 Int8
instance CanHoldSigned 'B8 Int16
instance CanHoldSigned 'B8 Int32
instance CanHoldSigned 'B8 Int64
instance CanHoldSigned 'B8 Int
instance CanHoldSigned 'B8 Integer
instance CanHoldSigned 'B16 Int16
instance CanHoldSigned 'B16 Int32
instance CanHoldSigned 'B16 Int64
instance CanHoldSigned 'B16 Int
instance CanHoldSigned 'B16 Integer
instance CanHoldSigned 'B32 Int32
instance CanHoldSigned 'B32 Int64
instance CanHoldSigned 'B32 Int
instance CanHoldSigned 'B32 Integer
instance CanHoldSigned 'B64 Int64
instance CanHoldSigned 'B64 Int
instance CanHoldSigned 'B64 Integer

type CanHoldUnsigned :: Bits -> * -> Constraint
class (BitBounds b, Num a) => CanHoldUnsigned b a where
instance CanHoldUnsigned 'B8 Word8
instance CanHoldUnsigned 'B8 Word16
instance CanHoldUnsigned 'B8 Word32
instance CanHoldUnsigned 'B8 Word64
instance CanHoldUnsigned 'B8 Word
instance CanHoldUnsigned 'B8 Integer
instance CanHoldUnsigned 'B8 Natural
instance CanHoldUnsigned 'B16 Word16
instance CanHoldUnsigned 'B16 Word32
instance CanHoldUnsigned 'B16 Word64
instance CanHoldUnsigned 'B16 Word
instance CanHoldUnsigned 'B16 Integer
instance CanHoldUnsigned 'B16 Natural
instance CanHoldUnsigned 'B32 Word32
instance CanHoldUnsigned 'B32 Word64
instance CanHoldUnsigned 'B32 Word
instance CanHoldUnsigned 'B32 Integer
instance CanHoldUnsigned 'B32 Natural
instance CanHoldUnsigned 'B64 Word64
instance CanHoldUnsigned 'B64 Word
instance CanHoldUnsigned 'B64 Integer
instance CanHoldUnsigned 'B64 Natural

type IntegerParsers :: (Bits -> * -> Constraint) -> *
data IntegerParsers canHold = IntegerParsers { decimal :: Parsec Integer
                                             , hexadecimal :: Parsec Integer
                                             , octal :: Parsec Integer
                                             , binary :: Parsec Integer
                                             , number :: Parsec Integer
                                             , decimal8 :: forall a. canHold 'B8 a => Parsec a
                                             , hexadecimal8 :: forall a. canHold 'B8 a => Parsec a
                                             , octal8 :: forall a. canHold 'B8 a => Parsec a
                                             , binary8 :: forall a. canHold 'B8 a => Parsec a
                                             , number8 :: forall a. canHold 'B8 a => Parsec a
                                             , decimal16 :: forall a. canHold 'B16 a => Parsec a
                                             , hexadecimal16 :: forall a. canHold 'B16 a => Parsec a
                                             , octal16 :: forall a. canHold 'B16 a => Parsec a
                                             , binary16 :: forall a. canHold 'B16 a => Parsec a
                                             , number16 :: forall a. canHold 'B16 a => Parsec a
                                             , decimal32 :: forall a. canHold 'B32 a => Parsec a
                                             , hexadecimal32 :: forall a. canHold 'B32 a => Parsec a
                                             , octal32 :: forall a. canHold 'B32 a => Parsec a
                                             , binary32 :: forall a. canHold 'B32 a => Parsec a
                                             , number32 :: forall a. canHold 'B32 a => Parsec a
                                             , decimal64 :: forall a. canHold 'B64 a => Parsec a
                                             , hexadecimal64 :: forall a. canHold 'B64 a => Parsec a
                                             , octal64 :: forall a. canHold 'B64 a => Parsec a
                                             , binary64 :: forall a. canHold 'B64 a => Parsec a
                                             , number64 :: forall a. canHold 'B64 a => Parsec a
                                             }

mkIntegerParsers :: forall (canHold :: (Bits -> * -> Constraint)).
                    (forall (bits :: Bits) t. canHold bits t => Proxy bits -> Parsec Integer -> Int -> Parsec t)
                 -> Parsec Integer
                 -> Parsec Integer
                 -> Parsec Integer
                 -> Parsec Integer
                 -> Parsec Integer
                 -> IntegerParsers canHold
mkIntegerParsers bounded decimal hexadecimal octal binary number = IntegerParsers {..}
  where decimalBounded :: forall (bits :: Bits) t. canHold bits t => Parsec t
        decimalBounded = bounded (Proxy @bits) decimal 10

        hexadecimalBounded :: forall (bits :: Bits) t. canHold bits t => Parsec t
        hexadecimalBounded = bounded (Proxy @bits) hexadecimal 16

        octalBounded :: forall (bits :: Bits) t. canHold bits t => Parsec t
        octalBounded = bounded (Proxy @bits) octal 8

        binaryBounded :: forall (bits :: Bits) t. canHold bits t => Parsec t
        binaryBounded = bounded (Proxy @bits) binary 2

        numberBounded :: forall (bits :: Bits) t. canHold bits t => Parsec t
        numberBounded = bounded (Proxy @bits) number 10

        decimal8 :: forall t. canHold 'B8 t => Parsec t
        decimal8 = decimalBounded @'B8
        decimal16 :: forall t. canHold 'B16 t => Parsec t
        decimal16 = decimalBounded @'B16
        decimal32 :: forall t. canHold 'B32 t => Parsec t
        decimal32 = decimalBounded @'B32
        decimal64 :: forall t. canHold 'B64 t => Parsec t
        decimal64 = decimalBounded @'B64

        hexadecimal8 :: forall t. canHold 'B8 t => Parsec t
        hexadecimal8 = hexadecimalBounded @'B8
        hexadecimal16 :: forall t. canHold 'B16 t => Parsec t
        hexadecimal16 = hexadecimalBounded @'B16
        hexadecimal32 :: forall t. canHold 'B32 t => Parsec t
        hexadecimal32 = hexadecimalBounded @'B32
        hexadecimal64 :: forall t. canHold 'B64 t => Parsec t
        hexadecimal64 = hexadecimalBounded @'B64

        octal8 :: forall t. canHold 'B8 t => Parsec t
        octal8 = octalBounded @'B8
        octal16 :: forall t. canHold 'B16 t => Parsec t
        octal16 = octalBounded @'B16
        octal32 :: forall t. canHold 'B32 t => Parsec t
        octal32 = octalBounded @'B32
        octal64 :: forall t. canHold 'B64 t => Parsec t
        octal64 = octalBounded @'B64

        binary8 :: forall t. canHold 'B8 t => Parsec t
        binary8 = binaryBounded @'B8
        binary16 :: forall t. canHold 'B16 t => Parsec t
        binary16 = binaryBounded @'B16
        binary32 :: forall t. canHold 'B32 t => Parsec t
        binary32 = binaryBounded @'B32
        binary64 :: forall t. canHold 'B64 t => Parsec t
        binary64 = binaryBounded @'B64

        number8 :: forall t. canHold 'B8 t => Parsec t
        number8 = numberBounded @'B8
        number16 :: forall t. canHold 'B16 t => Parsec t
        number16 = numberBounded @'B16
        number32 :: forall t. canHold 'B32 t => Parsec t
        number32 = numberBounded @'B32
        number64 :: forall t. canHold 'B64 t => Parsec t
        number64 = numberBounded @'B64

mkUnsigned :: NumericDesc -> GenericNumeric -> IntegerParsers CanHoldUnsigned
mkUnsigned desc@NumericDesc{..} gen = mkIntegerParsers bounded decimal hexadecimal octal binary number
  where bounded :: forall (bits :: Bits) t. CanHoldUnsigned bits t
                => Proxy bits -> Parsec Integer -> Int -> Parsec t
        bounded _ num _radix = mapMaybeS
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
mkSigned NumericDesc{..} unsigned =
  mkIntegerParsers bounded _decimal _hexadecimal _octal _binary _number
  where bounded :: forall (bits :: Bits) t. CanHoldSigned bits t
                => Proxy bits -> Parsec Integer -> Int -> Parsec t
        bounded _ num _radix = mapMaybeS
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

type FloatingParsers :: *
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
mkSignedCombined NumericDesc{..} unsigned = CombinedParsers {}

lexemeInteger :: (forall a. Parsec a -> Parsec a) -> IntegerParsers c -> IntegerParsers c
lexemeInteger = const id

lexemeFloating :: (forall a. Parsec a -> Parsec a) -> FloatingParsers -> FloatingParsers
lexemeFloating = const id

lexemeCombined :: (forall a. Parsec a -> Parsec a) -> CombinedParsers -> CombinedParsers
lexemeCombined = const id
