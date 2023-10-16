module Text.Gigaparsec.Internal.PlainString where

import Text.Gigaparsec
import Text.Gigaparsec.Char (string)

import Data.String

instance s ~ String => IsString (Parsec s) where fromString = string
