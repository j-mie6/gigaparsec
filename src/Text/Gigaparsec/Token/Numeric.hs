{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, ApplicativeDo #-}
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
