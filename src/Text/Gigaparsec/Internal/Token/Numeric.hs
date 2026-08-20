{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds, ConstraintKinds, AllowAmbiguousTypes, KindSignatures, MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Numeric (module Text.Gigaparsec.Internal.Token.Numeric) where

import Text.Gigaparsec (Parsec, unit, void, atomic, (<|>), ($>), many, (<:>))
import Text.Gigaparsec.Char (char, oneOf, digit, hexDigit, octDigit, bit)
import Text.Gigaparsec.Combinator (optional, optionalAs)
import Text.Gigaparsec.Token.Descriptions
    ( BreakCharDesc(BreakCharSupported, NoBreakChar),
      NumericDesc( NumericDesc, positiveSign, literalBreakChar
                 , integerNumbersCanBeHexadecimal, integerNumbersCanBeOctal
                 , integerNumbersCanBeBinary
                 , realNumbersCanBeHexadecimal, realNumbersCanBeOctal, realNumbersCanBeBinary
                 , hexadecimalLeads, octalLeads, binaryLeads
                 , leadingDotAllowed, trailingDotAllowed
                 , decimalExponentDesc, hexadecimalExponentDesc
                 , octalExponentDesc, binaryExponentDesc
                 ),
      ExponentDesc(NoExponents, ExponentsSupported, compulsory, chars, base, expSign, expLeadingZerosAllowd),
      PlusSignPresence(PlusIllegal, PlusRequired, PlusOptional) )
import Text.Gigaparsec.Internal.Token.Generic (
    GenericNumeric( plainDecimal, plainHexadecimal, plainOctal, plainBinary
                  , zeroAllowedDecimal, zeroAllowedHexadecimal, zeroAllowedOctal, zeroAllowedBinary
                  , zeroNotAllowedDecimal )
  )
import Text.Gigaparsec.Internal.Token.BitBounds (
    CanHoldUnsigned, CanHoldSigned,
    BitBounds(upperSigned, upperUnsigned, lowerSigned),
    Bits(B8, B16, B32, B64), bits
  )
import Text.Gigaparsec.Token.Errors (
    ErrorConfig ( filterIntegerOutOfBounds
                , labelIntegerSignedDecimal, labelIntegerUnsignedDecimal
                , labelIntegerSignedHexadecimal, labelIntegerUnsignedHexadecimal
                , labelIntegerSignedOctal, labelIntegerUnsignedOctal
                , labelIntegerSignedBinary, labelIntegerUnsignedBinary
                , labelIntegerSignedNumber, labelIntegerUnsignedNumber
                , labelIntegerDecimalEnd, labelIntegerHexadecimalEnd
                , labelIntegerOctalEnd, labelIntegerBinaryEnd, labelIntegerNumberEnd
                , labelRealDecimal, labelRealHexadecimalEnd, labelRealOctalEnd
                , labelRealBinaryEnd, labelRealNumberEnd
                , labelRealDot, labelRealExponent, labelRealExponentEnd
                , preventRealDoubleDroppedZero
                , labelNumericBreakChar )
  )
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import Data.Ratio ((%))
import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Bifunctor (bimap)
import Control.Monad (when, unless)
import Text.Gigaparsec.Internal.Token.Errors ( mapMaybeS, LabelWithExplainConfig, annotate
                                             , LabelConfig(LNotConfigured)
                                             , PreventDotIsZeroConfig(UnexpectedZeroDot, UnexpectedZeroDotWithReason, ZeroDotReason, ZeroDotFail) )
import Text.Gigaparsec.Errors.Combinator (filterOut, guardAgainst, unexpectedWhen, unexpectedWithReasonWhen)

-- TODO: switch to private versions in future
{-|
A uniform interface for defining parsers for integer literals, 
independent of how whitespace should be handled after the literal 
or whether the literal should allow for negative numbers.
-}
type IntegerParsers :: (Bits -> * -> Constraint) -> *
data IntegerParsers canHold = IntegerParsers { 
  -- | Parse a single integer literal in decimal form (base 10).
    decimal :: Parsec Integer
  -- | Parse a single integer literal in hexadecimal form (base 16).
  , hexadecimal :: Parsec Integer
  -- | Parse a single integer literal in octal form (base 8).
  , octal :: Parsec Integer
  -- | Parse a single integer literal in binary form (base 2).
  , binary :: Parsec Integer
  -- | Parse a single integer literal, 
  -- which can be in many forms and bases depending on the configuration.
  , number :: Parsec Integer
  , _bounded :: forall (bits :: Bits) t. canHold bits t
            => Proxy bits
            -> Parsec Integer
            -> Int
            -> (ErrorConfig -> Bool -> Maybe Bits -> LabelWithExplainConfig)
            -> Parsec t
  }

decimalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
decimalBounded IntegerParsers{..} = _bounded (Proxy @bits) decimal 10 label
  where label !err True = labelIntegerSignedDecimal err
        label err False = labelIntegerUnsignedDecimal err

hexadecimalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
hexadecimalBounded IntegerParsers{..} = _bounded (Proxy @bits) hexadecimal 16 label
  where label !err True = labelIntegerSignedHexadecimal err
        label err False = labelIntegerUnsignedHexadecimal err

octalBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
octalBounded IntegerParsers{..} = _bounded (Proxy @bits) octal 8 label
  where label !err True  = labelIntegerSignedOctal err
        label err False = labelIntegerUnsignedOctal err

binaryBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
binaryBounded IntegerParsers{..} = _bounded (Proxy @bits) binary 2 label
  where label !err True = labelIntegerSignedBinary err
        label err False = labelIntegerUnsignedBinary err

numberBounded :: forall (bits :: Bits) canHold t. canHold bits t => IntegerParsers canHold -> Parsec t
numberBounded IntegerParsers{..} = _bounded (Proxy @bits) number 10 label
  where label !err True = labelIntegerSignedNumber err
        label err False = labelIntegerUnsignedNumber err

{-|
This parser behaves the same as 'decimal' except it ensures that the resulting value is a valid 8-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
decimal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
decimal8 = decimalBounded @'B8
{-|
This parser behaves the same as 'hexadecimal' except it ensures that the resulting value is a valid 8-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
hexadecimal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
hexadecimal8 = hexadecimalBounded @'B8
{-|
This parser behaves the same as 'octal' except it ensures that the resulting value is a valid 8-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
octal8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
octal8 = octalBounded @'B8
{-|
This parser behaves the same as 'binary' except it ensures that the resulting value is a valid 8-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
binary8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
binary8 = binaryBounded @'B8
{-|
This parser behaves the same as 'number' except it ensures that the resulting value is a valid 8-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
number8 :: forall a canHold. canHold 'B8 a => IntegerParsers canHold -> Parsec a
number8 = numberBounded @'B8


---------------------------------------------------------------------------------------------------
-- *** 16-bit Parsers

{-|
This parser behaves the same as 'decimal' except it ensures that the resulting value is a valid 16-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
decimal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
decimal16 = decimalBounded @'B16
{-|
This parser behaves the same as 'hexadecimal' except it ensures that the resulting value is a valid 16-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
hexadecimal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
hexadecimal16 = hexadecimalBounded @'B16
{-|
This parser behaves the same as 'octal' except it ensures that the resulting value is a valid 16-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
octal16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
octal16 = octalBounded @'B16
{-|
This parser behaves the same as 'binary' except it ensures that the resulting value is a valid 16-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
binary16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
binary16 = binaryBounded @'B16
{-|
This parser behaves the same as 'number' except it ensures that the resulting value is a valid 16-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
number16 :: forall a canHold. canHold 'B16 a => IntegerParsers canHold -> Parsec a
number16 = numberBounded @'B16


---------------------------------------------------------------------------------------------------
-- *** 32-bit Parsers
{-|
This parser behaves the same as 'decimal' except it ensures that the resulting value is a valid 32-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
decimal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
decimal32 = decimalBounded @'B32
{-|
This parser behaves the same as 'hexadecimal' except it ensures that the resulting value is a valid 32-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
hexadecimal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
hexadecimal32 = hexadecimalBounded @'B32
{-|
This parser behaves the same as 'octal' except it ensures that the resulting value is a valid 32-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
octal32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
octal32 = octalBounded @'B32
{-|
This parser behaves the same as 'binary' except it ensures that the resulting value is a valid 32-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
binary32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
binary32 = binaryBounded @'B32
{-|
This parser behaves the same as 'number' except it ensures that the resulting value is a valid 32-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
number32 :: forall a canHold. canHold 'B32 a => IntegerParsers canHold -> Parsec a
number32 = numberBounded @'B32


---------------------------------------------------------------------------------------------------
-- *** 64-bit Parsers

{-|
This parser behaves the same as 'decimal' except it ensures that the resulting value is a valid 64-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
decimal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
decimal64 = decimalBounded @'B64
{-|
This parser behaves the same as 'hexadecimal' except it ensures that the resulting value is a valid 64-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
hexadecimal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
hexadecimal64 = hexadecimalBounded @'B64
{-|
This parser behaves the same as 'octal' except it ensures that the resulting value is a valid 64-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
octal64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
octal64 = octalBounded @'B64
{-|
This parser behaves the same as 'binary' except it ensures that the resulting value is a valid 64-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
binary64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
binary64 = binaryBounded @'B64
{-|
This parser behaves the same as 'number' except it ensures that the resulting value is a valid 64-bit number.

The resulting number will be converted to the given type @a@, which must be able to losslessly store the parsed value; 
this is enforced by the @canHold@ constraint on the type. 
This accounts for unsignedness when necessary.
-}
number64 :: forall a canHold. canHold 'B64 a => IntegerParsers canHold -> Parsec a
number64 = numberBounded @'B64

mkUnsigned :: NumericDesc -> GenericNumeric -> ErrorConfig -> IntegerParsers CanHoldUnsigned
mkUnsigned desc@NumericDesc{..} !gen !err = IntegerParsers {..}
  where _bounded :: forall (b :: Bits) t. CanHoldUnsigned b t
                 => Proxy b -> Parsec Integer -> Int
                 -> (ErrorConfig -> Bool -> Maybe Bits -> LabelWithExplainConfig)
                 -> Parsec t
        _bounded _ num radix label = annotate (label err False (Just (bits @b))) $
          mapMaybeS (filterIntegerOutOfBounds err 0 (upperUnsigned @b) radix)
                    (\n -> if n >= 0 && n <= upperUnsigned @b then Just (fromInteger n) else Nothing)
                    num

        leadingBreakChar = case literalBreakChar of
          NoBreakChar -> unit
          BreakCharSupported breakChar allowedAfterNonDecimalPrefix ->
            when allowedAfterNonDecimalPrefix (optional (char breakChar))

        noZeroHexadecimal = do
          unless (null hexadecimalLeads) (void (oneOf hexadecimalLeads))
          leadingBreakChar
          annotate (labelIntegerHexadecimalEnd err) (plainHexadecimal gen desc (labelIntegerHexadecimalEnd err))

        noZeroOctal = do
          unless (null octalLeads) (void (oneOf octalLeads))
          leadingBreakChar
          annotate (labelIntegerOctalEnd err) (plainOctal gen desc (labelIntegerOctalEnd err))

        noZeroBinary = do
          unless (null binaryLeads) (void (oneOf binaryLeads))
          leadingBreakChar
          annotate (labelIntegerBinaryEnd err) (plainBinary gen desc (labelIntegerBinaryEnd err))

        decimal = annotate (labelIntegerUnsignedDecimal err Nothing) $ plainDecimal gen desc (labelIntegerDecimalEnd err)
        hexadecimal = annotate (labelIntegerUnsignedHexadecimal err Nothing) $ atomic (char '0' *> noZeroHexadecimal)
        octal = annotate (labelIntegerUnsignedOctal err Nothing) $ atomic (char '0' *> noZeroOctal)
        binary = annotate (labelIntegerUnsignedBinary err Nothing) $ atomic (char '0' *> noZeroBinary)
        -- FIXME: numberEnd label is not applied here!
        number
          | not integerNumbersCanBeBinary
          , not integerNumbersCanBeHexadecimal
          , not integerNumbersCanBeOctal = annotate (labelIntegerUnsignedNumber err Nothing) decimal
          | otherwise = annotate (labelIntegerUnsignedNumber err Nothing) $ atomic (zeroLead <|> decimal)
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

mkSigned :: NumericDesc -> IntegerParsers c -> ErrorConfig -> IntegerParsers CanHoldSigned
mkSigned NumericDesc{..} !unsigned !err = IntegerParsers {
    decimal = _decimal,
    hexadecimal = _hexadecimal,
    octal = _octal,
    binary = _binary,
    number = _number,
    ..
  }
  where _bounded :: forall (b :: Bits) t. CanHoldSigned b t
                 => Proxy b -> Parsec Integer -> Int
                 -> (ErrorConfig -> Bool -> Maybe Bits -> LabelWithExplainConfig)
                 -> Parsec t
        _bounded _ num radix label = annotate (label err True (Just (bits @b))) $
          mapMaybeS (filterIntegerOutOfBounds err (lowerSigned @b) (upperSigned @b) radix)
                    (\n -> if n >= lowerSigned @b && n <= upperSigned @b
                           then Just (fromInteger n)
                           else Nothing)
                    num

        sign :: Parsec (Integer -> Integer)
        sign = case positiveSign of
          PlusRequired -> char '+' $> id <|> char '-' $> negate
          PlusOptional -> char '-' $> negate <|> optionalAs id (char '+')
          PlusIllegal  -> pure id
        _decimal = annotate (labelIntegerSignedDecimal err Nothing) $
          atomic (sign <*> annotate (labelIntegerDecimalEnd err) (decimal unsigned))
        _hexadecimal = annotate (labelIntegerSignedHexadecimal err Nothing) $
          atomic (sign <*> annotate (labelIntegerHexadecimalEnd err) (hexadecimal unsigned))
        _octal = annotate (labelIntegerSignedOctal err Nothing) $
          atomic (sign <*> annotate (labelIntegerOctalEnd err) (octal unsigned))
        _binary = annotate (labelIntegerSignedBinary err Nothing) $
          atomic (sign <*> annotate (labelIntegerBinaryEnd err) (binary unsigned))
        _number = annotate (labelIntegerSignedNumber err Nothing) $
          atomic (sign <*> annotate (labelIntegerNumberEnd err) (number unsigned))

type FloatingParsers :: *
data FloatingParsers = FloatingParsers { 
  -- | Parse a single real number literal in decimal form (base 10).
    decimalFloating :: Parsec Rational
  -- | Parse a single real number literal in hexadecimal form (base 16).
  , hexadecimalFloating :: Parsec Rational
  -- | Parse a single real number literal in octal form (base 8).
  , octalFloating :: Parsec Rational
  -- | Parse a single real number literal in binary form (base 2).
  , binaryFloating :: Parsec Rational
  -- | Parse a single real number literal, 
  -- which can be in many forms and bases depending on the configuration.
  , numberFloating :: Parsec Rational
  }

mkUnsignedFloating :: NumericDesc -> IntegerParsers CanHoldUnsigned -> GenericNumeric -> ErrorConfig -> FloatingParsers
mkUnsignedFloating NumericDesc{..} !nat !gen !err = FloatingParsers {..}
  where
    foldFrac :: Integer -> [Char] -> Rational
    foldFrac radix ds = case foldl' go (0, 1) ds of { (n, p) -> n % p }
      where go (n, p) d = (n * radix + fromIntegral (digitToInt d), p * radix)

    collectFracDigits :: Parsec Char -> LabelConfig -> Bool -> Parsec [Char]
    collectFracDigits dig endLabel allowEmpty = case literalBreakChar of
      NoBreakChar
        | allowEmpty -> many (annotate endLabel dig)
        | otherwise  -> annotate endLabel dig <:> many (annotate endLabel dig)
      BreakCharSupported bc _
        | allowEmpty -> many (optional (annotate (labelNumericBreakChar err) (char bc)) *> annotate endLabel dig)
        | otherwise  -> annotate endLabel dig <:> many (optional (annotate (labelNumericBreakChar err) (char bc)) *> annotate endLabel dig)

    applyPrevent :: Parsec ()
    applyPrevent = case preventRealDoubleDroppedZero err of
      UnexpectedZeroDot msg ->
        unexpectedWhen (const (Just msg)) (pure ())
      UnexpectedZeroDotWithReason msg rsn ->
        unexpectedWithReasonWhen (const (Just (msg, rsn))) (pure ())
      ZeroDotReason rsn ->
        filterOut (const (Just rsn)) (pure ())
      ZeroDotFail msgs ->
        guardAgainst (const (Just msgs)) (pure ())

    parseExponent :: ExponentDesc -> Parsec (Rational -> Rational)
    parseExponent NoExponents = pure id
    parseExponent ExponentsSupported{ compulsory = compulsory, chars = expChars, base = expBase
                                    , expSign = expSign, expLeadingZerosAllowd = expLeadingZerosAllowd } =
      let expDigits = if expLeadingZerosAllowd
                        then zeroAllowedDecimal gen (labelRealExponentEnd err)
                        else zeroNotAllowedDecimal gen (labelRealExponentEnd err)
          expSignParser :: Parsec (Integer -> Integer)
          expSignParser = case expSign of
            PlusRequired -> char '+' $> id <|> char '-' $> negate
            PlusOptional -> char '-' $> negate <|> optionalAs id (char '+')
            PlusIllegal  -> pure id
          expParser = annotate (labelRealExponent err) $ do
            void (oneOf expChars)
            applySign <- expSignParser
            n <- expDigits
            let e = applySign n
            pure $ if e >= 0
                     then (* fromInteger (toInteger expBase ^ e))
                     else (/ fromInteger (toInteger expBase ^ negate e))
      in if compulsory then expParser else expParser <|> pure id

    floatBody :: Integer -> Parsec Integer -> Parsec Char -> LabelConfig -> ExponentDesc -> Parsec Rational
    floatBody radix intP dig endLabel expDesc = do
      mInt <- if leadingDotAllowed then optionalAs Nothing (fmap Just intP) else fmap Just intP
      void (annotate (labelRealDot err) (char '.'))
      fracs <- collectFracDigits dig endLabel trailingDotAllowed
      when (leadingDotAllowed && trailingDotAllowed && null mInt && null fracs) applyPrevent
      let sig = maybe 0 fromInteger mInt + foldFrac radix fracs
      expFn <- parseExponent expDesc
      pure (expFn sig)

    leadingBreakChar :: Parsec ()
    leadingBreakChar = case literalBreakChar of
      NoBreakChar -> unit
      BreakCharSupported breakChar allowedAfterNonDecimalPrefix ->
        when allowedAfterNonDecimalPrefix (optional (char breakChar))

    noZeroHexFloat :: Parsec Rational
    noZeroHexFloat = do
      unless (null hexadecimalLeads) (void (oneOf hexadecimalLeads))
      leadingBreakChar
      floatBody 16 (zeroAllowedHexadecimal gen (labelRealHexadecimalEnd err))
                hexDigit (labelRealHexadecimalEnd err) hexadecimalExponentDesc

    noZeroOctalFloat :: Parsec Rational
    noZeroOctalFloat = do
      unless (null octalLeads) (void (oneOf octalLeads))
      leadingBreakChar
      floatBody 8 (zeroAllowedOctal gen (labelRealOctalEnd err))
               octDigit (labelRealOctalEnd err) octalExponentDesc

    noZeroBinaryFloat :: Parsec Rational
    noZeroBinaryFloat = do
      unless (null binaryLeads) (void (oneOf binaryLeads))
      leadingBreakChar
      floatBody 2 (zeroAllowedBinary gen (labelRealBinaryEnd err))
               bit (labelRealBinaryEnd err) binaryExponentDesc

    decimalFloating :: Parsec Rational
    decimalFloating = annotate (labelRealDecimal err) $ atomic $
      floatBody 10 (decimal nat) digit LNotConfigured decimalExponentDesc

    hexadecimalFloating :: Parsec Rational
    hexadecimalFloating = annotate (labelRealHexadecimalEnd err) $ atomic (char '0' *> noZeroHexFloat)

    octalFloating :: Parsec Rational
    octalFloating = annotate (labelRealOctalEnd err) $ atomic (char '0' *> noZeroOctalFloat)

    binaryFloating :: Parsec Rational
    binaryFloating = annotate (labelRealBinaryEnd err) $ atomic (char '0' *> noZeroBinaryFloat)

    numberFloating :: Parsec Rational
    numberFloating
      | not realNumbersCanBeBinary
      , not realNumbersCanBeHexadecimal
      , not realNumbersCanBeOctal =
          annotate (labelRealNumberEnd err) decimalFloating
      | otherwise = annotate (labelRealNumberEnd err) $ atomic $
          addHex (addOct (addBin decimalFloating))
      where
        addHex | realNumbersCanBeHexadecimal = (hexadecimalFloating <|>)
               | otherwise = id
        addOct | realNumbersCanBeOctal = (octalFloating <|>)
               | otherwise = id
        addBin | realNumbersCanBeBinary = (binaryFloating <|>)
               | otherwise = id

mkSignedFloating :: NumericDesc -> FloatingParsers -> ErrorConfig -> FloatingParsers
mkSignedFloating NumericDesc{positiveSign = positiveSign} !unsigned !_err = FloatingParsers {
    decimalFloating     = signed (decimalFloating unsigned),
    hexadecimalFloating = signed (hexadecimalFloating unsigned),
    octalFloating       = signed (octalFloating unsigned),
    binaryFloating      = signed (binaryFloating unsigned),
    numberFloating      = signed (numberFloating unsigned)
  }
  where
    sign :: Parsec (Rational -> Rational)
    sign = case positiveSign of
      PlusRequired -> char '+' $> id <|> char '-' $> negate
      PlusOptional -> char '-' $> negate <|> optionalAs id (char '+')
      PlusIllegal  -> pure id
    signed :: Parsec Rational -> Parsec Rational
    signed p = atomic (sign <*> p)

type CombinedParsers :: *
data CombinedParsers = CombinedParsers {
  -- | Parse a single literal in decimal form (base 10), yielding an integer or a real number.
    decimalCombined :: Parsec (Either Integer Rational)
  -- | Parse a single literal in hexadecimal form (base 16), yielding an integer or a real number.
  , hexadecimalCombined :: Parsec (Either Integer Rational)
  -- | Parse a single literal in octal form (base 8), yielding an integer or a real number.
  , octalCombined :: Parsec (Either Integer Rational)
  -- | Parse a single literal in binary form (base 2), yielding an integer or a real number.
  , binaryCombined :: Parsec (Either Integer Rational)
  -- | Parse a single literal, which can be in many forms and bases depending on the configuration,
  -- yielding an integer or a real number.
  , numberCombined :: Parsec (Either Integer Rational)
  }

mkUnsignedCombined :: NumericDesc -> IntegerParsers CanHoldUnsigned -> FloatingParsers -> CombinedParsers
mkUnsignedCombined _desc !nat !fl = CombinedParsers {
    decimalCombined     = combinedOf (decimalFloating fl)     (decimal nat),
    hexadecimalCombined = combinedOf (hexadecimalFloating fl) (hexadecimal nat),
    octalCombined       = combinedOf (octalFloating fl)       (octal nat),
    binaryCombined      = combinedOf (binaryFloating fl)      (binary nat),
    numberCombined      = combinedOf (numberFloating fl)      (number nat)
  }
  where
    combinedOf :: Parsec Rational -> Parsec Integer -> Parsec (Either Integer Rational)
    combinedOf real int = fmap Right (atomic real) <|> fmap Left int

mkSignedCombined :: NumericDesc -> CombinedParsers -> CombinedParsers
mkSignedCombined NumericDesc{positiveSign = positiveSign} !unsigned = CombinedParsers {
    decimalCombined     = signed (decimalCombined unsigned),
    hexadecimalCombined = signed (hexadecimalCombined unsigned),
    octalCombined       = signed (octalCombined unsigned),
    binaryCombined      = signed (binaryCombined unsigned),
    numberCombined      = signed (numberCombined unsigned)
  }
  where
    sign :: Parsec (Either Integer Rational -> Either Integer Rational)
    sign = case positiveSign of
      PlusRequired -> char '+' $> id      <|> char '-' $> bimap negate negate
      PlusOptional -> char '-' $> bimap negate negate <|> optionalAs id (char '+')
      PlusIllegal  -> pure id
    signed :: Parsec (Either Integer Rational) -> Parsec (Either Integer Rational)
    signed p = atomic (sign <*> p)

lexemeInteger :: (forall a. Parsec a -> Parsec a) -> IntegerParsers c -> IntegerParsers c
lexemeInteger lexe IntegerParsers{..} = IntegerParsers {
    decimal = lexe decimal,
    hexadecimal = lexe hexadecimal,
    octal = lexe octal,
    binary = lexe binary,
    number = lexe number,
    _bounded = \n b radix label -> lexe (_bounded n b radix label)
  }

lexemeFloating :: (forall a. Parsec a -> Parsec a) -> FloatingParsers -> FloatingParsers
lexemeFloating lexe FloatingParsers{..} = FloatingParsers {
    decimalFloating    = lexe decimalFloating,
    hexadecimalFloating = lexe hexadecimalFloating,
    octalFloating      = lexe octalFloating,
    binaryFloating     = lexe binaryFloating,
    numberFloating     = lexe numberFloating
  }

lexemeCombined :: (forall a. Parsec a -> Parsec a) -> CombinedParsers -> CombinedParsers
lexemeCombined lexe CombinedParsers{..} = CombinedParsers {
    decimalCombined     = lexe decimalCombined,
    hexadecimalCombined = lexe hexadecimalCombined,
    octalCombined       = lexe octalCombined,
    binaryCombined      = lexe binaryCombined,
    numberCombined      = lexe numberCombined
  }
