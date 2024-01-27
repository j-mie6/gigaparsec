{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
-- Yes, this is redundant, however, it is necessary to get the UNPACK to fire on CaretWidth
{-# OPTIONS_GHC -Wno-redundant-strictness-flags #-}
module Text.Gigaparsec.Internal.Errors.ErrorItem (
    module Text.Gigaparsec.Internal.Errors.ErrorItem
  ) where

import Data.List.NonEmpty (NonEmpty)

import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth)

type Input :: *
type Input = NonEmpty Char
type UnexpectItem :: *
data UnexpectItem = UnexpectRaw !Input {-# UNPACK #-} !Word
                  | UnexpectNamed !String {-# UNPACK #-} !CaretWidth
                  | UnexpectEndOfInput
type ExpectItem :: *
data ExpectItem = ExpectRaw !String
                | ExpectNamed !String
                | ExpectEndOfInput
                deriving stock (Eq, Ord, Show)
