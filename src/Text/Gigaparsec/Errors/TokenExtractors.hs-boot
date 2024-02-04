{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Errors.TokenExtractors (module Text.Gigaparsec.Errors.TokenExtractors) where

import Data.List.NonEmpty (NonEmpty)

type TokenExtractor :: *
type TokenExtractor = NonEmpty Char -> Word -> Bool -> Token

type Token :: *
data Token = Raw !String | Named !String {-# UNPACK #-} !Word

tillNextWhitespace :: Bool -> (Char -> Bool) -> TokenExtractor
