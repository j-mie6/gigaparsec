{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UnicodeSyntax, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Gigaparsec.Internal.Input where

import Data.Kind (Constraint)
import Data.List (uncons)
import Control.Arrow (second)

-- TODO: add a NonEmptyInputStream subclass of InputStream
type InputStream :: * -> Constraint
class (Eq s, Show s) => InputStream s where
  -- | 'True' when the input stream is empty/has reached the end
  isEmptyInputStream :: s -> Bool
  -- | Read a file and return its contents as an @s@.
  -- TODO: perhaps we should also parametrise on *where* the input comes from?
  readInputStream :: FilePath -> IO s 

  toStringInputStream :: s -> String

  unconsInputStream :: s -> Maybe (Char, s)

  -- -- | It is best this is directly implemented
  -- satisfyInput :: s -> Parsec Char

type Input :: *
data Input = ∀ s . InputStream s => Input !s

deriving stock instance Show Input

{-|
This is obviously hideously expensive.
-}
instance Eq Input where
  (Input x) == (Input y) = toStringInputStream x == toStringInputStream y

{-# INLINE isEmptyInput #-}
isEmptyInput :: Input -> Bool
isEmptyInput (Input x) = isEmptyInputStream x

{-| Read input from a file.

Note: Requires a type application for the 'InputStream' type
-}
{-# INLINE readInput #-}
readInput :: ∀ s . InputStream s => FilePath -> IO Input
readInput = (Input @s <$>) . readInputStream @s


{-# INLINE inputToString #-}
inputToString :: Input -> String
inputToString (Input x) = toStringInputStream x

-- Could just use Control.Arrow.second, but idk if this has the right strictness properties
{-# INLINABLE unconsInput #-}
unconsInput :: Input -> Maybe (Char, Input)
unconsInput (Input x) = (second Input) <$> unconsInputStream x
  -- case unconsInputStream x of
  -- Just (!c, !x') -> Just (c, Input x')
  -- Nothing -> Nothing

instance InputStream String where
  {-# INLINE isEmptyInputStream #-}
  isEmptyInputStream = null

  {-# INLINE readInputStream #-}
  readInputStream = readFile

  {-# INLINE unconsInputStream #-}
  unconsInputStream = uncons

  {-# INLINE toStringInputStream #-}
  toStringInputStream = id

stringInput :: String -> Input
stringInput = Input
