{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Errors.ErrorBuilder (module Text.Gigaparsec.Errors.ErrorBuilder) where

import Prelude hiding (lines)

import Data.Monoid (Endo(Endo))
import Data.String (IsString(fromString))
import Data.List (intersperse)

-- For now, this is the home of the default formatting functions

type StringBuilder :: *
newtype StringBuilder = StringBuilder (String -> String)
  deriving (Semigroup, Monoid) via Endo String

instance IsString StringBuilder where
  fromString str = StringBuilder (str ++)

toString :: StringBuilder -> String
toString (StringBuilder build) = build mempty

from :: Show a => a -> StringBuilder
from = StringBuilder . shows

formatDefault :: StringBuilder -> Maybe StringBuilder -> [StringBuilder] -> String
formatDefault pos source lines = toString (blockError header lines 2)
  where header = maybe mempty (\src -> "In " <> src <> " ") source <> pos

blockError :: StringBuilder -> [StringBuilder] -> Int -> StringBuilder
blockError header lines indent = header <> ":\n" <> indentAndUnlines lines indent

indentAndUnlines :: [StringBuilder] -> Int -> StringBuilder
indentAndUnlines lines indent =
  mconcat (fromString pre : intersperse (fromString ('\n' : pre)) lines)
  where pre = replicate indent ' '

formatPosDefault :: Word -> Word -> StringBuilder
formatPosDefault line col = "(line "
                         <> from line
                         <> ", column "
                         <> from col
                         <> ")"
