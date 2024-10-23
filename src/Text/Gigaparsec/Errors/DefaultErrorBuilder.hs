{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-|
Module      : Text.Gigaparsec.Errors.DefaultErrorBuilder
Description : This module defines Gigaparsec's default error messages.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module defines Gigaparsec's default error messages.
The actual 'Text.Gigaparsec.Errors.ErrorBuilder.ErrorBuilder' class instance is found in 
"Text.Gigaparsec.Errors.ErrorBuilder".

-}
module Text.Gigaparsec.Errors.DefaultErrorBuilder (module Text.Gigaparsec.Errors.DefaultErrorBuilder) where

import Prelude hiding (lines)

import Data.Monoid (Endo(Endo))
import Data.String (IsString(fromString))
import Data.List (intersperse, sortBy)
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Ord (comparing, Down (Down))

-- For now, this is the home of the default formatting functions

-- | A string-builder is an efficient way of constructing a string through a series of concatenations.
type StringBuilder :: *
newtype StringBuilder = StringBuilder (String -> String)
  deriving (Semigroup, Monoid) via Endo String

instance IsString StringBuilder where
  {-# INLINE fromString #-}
  fromString :: String -> StringBuilder
  fromString str = StringBuilder (str ++)

{-# INLINE toString #-}
-- | Runs the given string-builder, producing a string.
toString :: StringBuilder -> String
toString (StringBuilder build) = build mempty

{-# INLINE from #-}
-- | Create a string-builder which starts with the string representation of the given argument.
from :: Show a => a -> StringBuilder
from = StringBuilder . shows

{-|
Forms an error message with 'blockError', with two spaces of indentation and incorporating 
the source file and position into the header.
-}
{-# INLINABLE buildDefault #-}
buildDefault :: StringBuilder -> Maybe StringBuilder -> [StringBuilder] -> String
buildDefault pos source lines = toString (blockError header lines 2)
  where header = maybe mempty (\src -> "In " <> src <> " ") source <> pos

{-|
Forms a vanilla error by combining all the components in sequence, if there is no information other than the lines,
"unknown parse error" is used instead.
-}
{-# INLINABLE vanillaErrorDefault #-}
vanillaErrorDefault :: Foldable t => Maybe StringBuilder -> Maybe StringBuilder -> t StringBuilder -> [StringBuilder] -> [StringBuilder]
vanillaErrorDefault unexpected expected reasons =
  combineInfoWithLines (maybe id (:) unexpected (maybe id (:) expected (toList reasons)))

{-|
Forms a specialized error by combining all components in sequence, if there are no msgs, then "unknown parse error" is used instead.
-}
{-# INLINABLE specialisedErrorDefault #-}
specialisedErrorDefault :: [StringBuilder] -> [StringBuilder] -> [StringBuilder]
specialisedErrorDefault = combineInfoWithLines

{-|
Joins together the given sequences: if the first is empty, then "unknown parse error" is prepended onto lines instead.
-}
{-# INLINABLE combineInfoWithLines #-}
combineInfoWithLines :: [StringBuilder] -> [StringBuilder] -> [StringBuilder]
combineInfoWithLines [] lines = "unknown parse error" : lines
combineInfoWithLines info lines = info ++ lines

{-|
Encloses the item in double-quotes.
-}
--TODO: this needs to deal with whitespace and unprintables
{-
If the given item is either a whitespace character or is otherwise "unprintable", 
a special name is given to it, otherwise the item is enclosed in double-quotes.
-}
{-# INLINABLE rawDefault #-}
rawDefault :: String -> String
rawDefault n = "\"" <> n <> "\""

-- | Returns the given name unchanged.
{-# INLINABLE namedDefault #-}
namedDefault :: String -> String
namedDefault = id

-- | Simply displays "end of input"
{-# INLINABLE endOfInputDefault #-}
endOfInputDefault :: String
endOfInputDefault = "end of input"

-- | Returns the given message unchanged.
{-# INLINABLE messageDefault #-}
messageDefault :: String -> String
messageDefault = id

-- | Adds "expected " before the given alternatives, should they exist.
{-# INLINABLE expectedDefault #-}
expectedDefault :: Maybe StringBuilder -> Maybe StringBuilder
expectedDefault = fmap ("expected " <>)

-- | Adds "unexpected " before the unexpected item.
{-# INLINABLE unexpectedDefault #-}
unexpectedDefault :: Maybe String -> Maybe StringBuilder
unexpectedDefault = fmap (("unexpected " <>) . fromString)

{-|
Combines the alternatives, separated by commas/semicolons, with the final two separated by "or". 
If the elements contain a comma, then semicolon is used as the list separator.
-}
{-# INLINABLE disjunct #-}
disjunct :: Bool -> [String] -> Maybe StringBuilder
disjunct oxford elems = junct oxford elems "or"

{-|
Combines the alternatives, separated by commas/semicolons, with the final two separated by "or". 
An Oxford comma is added if there are more than two elements, as this helps prevent ambiguity in the list. 
If the elements contain a comma, then semicolon is used as the list separator.
-}
{-# INLINABLE junct #-}
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

{-|
Filters out any empty messages and returns the rest.
-}
{-# INLINABLE combineMessagesDefault #-}
combineMessagesDefault :: Foldable t => t String -> [StringBuilder]
combineMessagesDefault = mapMaybe (\msg -> if null msg then Nothing else Just (fromString msg)) . toList

{-|
Forms an error with the given header followed by a colon, a newline, then the remainder of the lines indented.
-}
{-# INLINABLE blockError #-}
blockError :: StringBuilder -> [StringBuilder] -> Int -> StringBuilder
blockError header lines indent = header <> ":\n" <> indentAndUnlines lines indent

{-|
Indents and concatenates the given lines by the given depth.
-}
{-# INLINABLE indentAndUnlines #-}
indentAndUnlines :: [StringBuilder] -> Int -> StringBuilder
indentAndUnlines lines indent = fromString pre <> intercalate (fromString ('\n' : pre)) lines
  where pre = replicate indent ' '

{-|
Constructs error context by concatenating them together with a "caret line" 
underneath the focus line, line, where the error occurs.
-}
{-# INLINABLE lineInfoDefault #-}
lineInfoDefault :: String -> [String] -> [String] -> Word -> Word -> Word -> [StringBuilder]
lineInfoDefault curLine beforeLines afterLines _line pointsAt width =
  concat [map inputLine beforeLines, [inputLine curLine, caretLine], map inputLine afterLines]
  where inputLine :: String -> StringBuilder
        inputLine = fromString . ('>' :)
        caretLine :: StringBuilder
        caretLine = fromString (replicate (fromIntegral (pointsAt + 1)) ' ') <> fromString (replicate (fromIntegral width) '^')

{-|
Pairs the line and column up in the form @(line m, column n)@.
-}
{-# INLINABLE posDefault #-}
posDefault :: Word -> Word -> StringBuilder
posDefault line col = "(line "
                   <> from line
                   <> ", column "
                   <> from col
                   <> ")"

{-# INLINABLE intercalate #-}
intercalate :: Monoid m => m -> [m] -> m
intercalate x xs = mconcat (intersperse x xs)
