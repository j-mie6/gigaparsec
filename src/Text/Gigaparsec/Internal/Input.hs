{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UnicodeSyntax, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Gigaparsec.Internal.Input where

import Data.Kind (Constraint)
import Data.List (uncons)

-- TODO: add a NonEmptyInputStream subclass of InputStream
type InputStream :: * -> Constraint
class InputStream s where
  -- | 'True' when the input stream is empty/has reached the end
  isEmptyInputStream :: s -> Bool
  -- | Read a file and return its contents as an @s@.
  -- TODO: perhaps we should also parametrise on *where* the input comes from?
  readInputStream :: FilePath -> IO s 

  toStringInputStream :: s -> String

  unconsInputStream :: s -> Maybe (Char, s) 

type Input :: *
data Input = ∀ s . InputStream s => Input !s

{-# INLINE isEmptyInput #-}
isEmptyInput :: Input -> Bool
isEmptyInput (Input x) = isEmptyInputStream x

{-# INLINE readInput #-}
{-| Read input from a file.

Note: Requires a type application for the 'InputStream' type
-}
readInput :: ∀ s . InputStream s => FilePath -> IO Input
readInput = (Input @s <$>) . readInputStream @s


{-# INLINE inputToString #-}
inputToString :: Input -> String
inputToString (Input x) = toStringInputStream x

-- Could just use Control.Arrow.second, but idk if this has the right strictness properties
{-# INLINABLE unconsInput #-}
unconsInput :: Input -> Maybe (Char, Input)
unconsInput (Input x) = case unconsInputStream x of
  Just (!c, !x') -> Just (c, Input x')
  Nothing -> Nothing

instance InputStream String where
  {-# INLINE isEmptyInputStream #-}
  isEmptyInputStream = ("" ==)

  {-# INLINE readInputStream #-}
  readInputStream = readFile

  {-# INLINE unconsInputStream #-}
  unconsInputStream = uncons

  {-# INLINE toStringInputStream #-}
  toStringInputStream = id

stringInput :: String -> Input
stringInput = Input
