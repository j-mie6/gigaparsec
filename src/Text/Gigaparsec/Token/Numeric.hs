{-# LANGUAGE Safe #-}
-- TODO: refine
module Text.Gigaparsec.Token.Numeric (module Text.Gigaparsec.Token.Numeric) where

import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Token.Descriptions
import Text.Gigaparsec.Token.Generic (GenericNumeric)

type IntegerParsers :: *
data IntegerParsers = IntegerParsers {}

mkUnsigned :: NumericDesc -> GenericNumeric -> IntegerParsers
mkUnsigned NumericDesc{..} gen = IntegerParsers {}

mkSigned :: NumericDesc -> IntegerParsers -> IntegerParsers
mkSigned NumericDesc{..} unsigned = IntegerParsers {}

type FloatingParsers :: *
data FloatingParsers = FloatingParsers {}

mkUnsignedFloating :: NumericDesc -> IntegerParsers -> GenericNumeric -> FloatingParsers
mkUnsignedFloating NumericDesc{..} nat gen = FloatingParsers {}

mkSignedFloating :: NumericDesc -> FloatingParsers -> FloatingParsers
mkSignedFloating NumericDesc{..} unsigned = FloatingParsers {}

type CombinedParsers :: *
data CombinedParsers = CombinedParsers {}

mkUnsignedCombined :: NumericDesc -> IntegerParsers -> FloatingParsers -> CombinedParsers
mkUnsignedCombined NumericDesc{..} natural floating = CombinedParsers {}

mkSignedCombined :: NumericDesc -> CombinedParsers -> CombinedParsers
mkSignedCombined NumericDesc{..} unsigned = CombinedParsers {}

lexemeInteger :: (forall a. Parsec a -> Parsec a) -> IntegerParsers -> IntegerParsers
lexemeInteger = const id

lexemeFloating :: (forall a. Parsec a -> Parsec a) -> FloatingParsers -> FloatingParsers
lexemeFloating = const id

lexemeCombined :: (forall a. Parsec a -> Parsec a) -> CombinedParsers -> CombinedParsers
lexemeCombined = const id
