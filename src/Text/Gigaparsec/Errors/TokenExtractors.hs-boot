{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Errors.TokenExtractors (module Text.Gigaparsec.Errors.TokenExtractors) where

import Data.List.NonEmpty (NonEmpty)

type TokenExtractor :: *
type TokenExtractor = NonEmpty Char -> Word -> Bool -> Token

{-|
This type represents an extracted token returned by 'unexpectedToken' in 'ErrorBuilder'.

There is deliberately no analogue for @EndOfInput@ because we guarantee that non-empty
residual input is provided to token extraction.
-}
type Token :: *
data Token = Raw                   -- ^ This is a token that is directly extracted from the residual input itself.
              !String              -- ^ the input extracted.
           | Named                 -- ^ This is a token that has been given a name, and is treated like a labelled item.
              !String              -- ^ the description of the token.
              {-# UNPACK #-} !Word -- ^ the amount of residual input this token ate.

tillNextWhitespace :: Bool -> (Char -> Bool) -> TokenExtractor
