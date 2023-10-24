{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Errors.ErrorBuilder (module Text.Gigaparsec.Errors.ErrorBuilder) where

import Prelude hiding (lines)

import Data.Monoid (Endo(Endo))
import Data.String (IsString(fromString))
import Data.List (intersperse, sortBy)
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Ord (comparing, Down (Down))

-- For now, this is the home of the default formatting functions

type StringBuilder :: *
newtype StringBuilder = StringBuilder (String -> String)
  deriving (Semigroup, Monoid) via Endo String

instance IsString StringBuilder where
  fromString :: String -> StringBuilder
  fromString str = StringBuilder (str ++)

toString :: StringBuilder -> String
toString (StringBuilder build) = build mempty

from :: Show a => a -> StringBuilder
from = StringBuilder . shows

formatDefault :: StringBuilder -> Maybe StringBuilder -> [StringBuilder] -> String
formatDefault pos source lines = toString (blockError header lines 2)
  where header = maybe mempty (\src -> "In " <> src <> " ") source <> pos

vanillaErrorDefault :: Foldable t => Maybe StringBuilder -> Maybe StringBuilder -> t StringBuilder -> [StringBuilder] -> [StringBuilder]
vanillaErrorDefault unexpected expected reasons =
  combineInfoWithLines (maybe id (:) unexpected (maybe id (:) expected (toList reasons)))

specialisedErrorDefault :: [StringBuilder] -> [StringBuilder] -> [StringBuilder]
specialisedErrorDefault = combineInfoWithLines

combineInfoWithLines :: [StringBuilder] -> [StringBuilder] -> [StringBuilder]
combineInfoWithLines [] lines = "unknown parse error" : lines
combineInfoWithLines info lines = info ++ lines

--TODO: this needs to deal with whitespace and unprintables
rawDefault :: String -> String
rawDefault n = "\"" <> n <> "\""

namedDefault :: String -> String
namedDefault = id

endOfInputDefault :: String
endOfInputDefault = "end of input"

messageDefault :: String -> String
messageDefault = id

expectedDefault :: Maybe StringBuilder -> Maybe StringBuilder
expectedDefault = fmap ("expected " <>)

unexpectedDefault :: Maybe String -> Maybe StringBuilder
unexpectedDefault = fmap (("unexpected " <>) . fromString)

disjunct :: Bool -> [String] -> Maybe StringBuilder
disjunct oxford elems = junct oxford elems "or"

junct :: Bool -> [String] -> String -> Maybe StringBuilder
junct oxford elems junction = junct' (sortBy (comparing Down) elems)
  where
    j :: StringBuilder
    j = fromString junction

    junct' [] = Nothing
    junct' [alt] = Just (fromString alt)
    junct' [alt1, alt2] = Just (fromString alt2 <> " " <> fromString junction <> " " <> fromString alt1)
    junct' as@(alt:alts)
      -- use a semi-colon here, it is more correct
      | any (elem ',') as = Just (junct'' (reverse alts) alt "; ")
      | otherwise         = Just (junct'' (reverse alts) alt ", ")

    junct'' is l delim = front <> back
      where front = intercalate (fromString delim) (map fromString is) :: StringBuilder
            back
              | oxford    = fromString delim <> j <> " " <> fromString l
              | otherwise = " " <> j <> " " <> fromString l

combineMessagesDefault :: Foldable t => t String -> [StringBuilder]
combineMessagesDefault = mapMaybe (\msg -> if null msg then Nothing else Just (fromString msg)) . toList

blockError :: StringBuilder -> [StringBuilder] -> Int -> StringBuilder
blockError header lines indent = header <> ":\n" <> indentAndUnlines lines indent

indentAndUnlines :: [StringBuilder] -> Int -> StringBuilder
indentAndUnlines lines indent = fromString pre <> intercalate (fromString ('\n' : pre)) lines
  where pre = replicate indent ' '

formatPosDefault :: Word -> Word -> StringBuilder
formatPosDefault line col = "(line "
                         <> from line
                         <> ", column "
                         <> from col
                         <> ")"

intercalate :: Monoid m => m -> [m] -> m
intercalate x xs = mconcat (intersperse x xs)
