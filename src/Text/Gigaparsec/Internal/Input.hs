{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UnicodeSyntax, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Text.Gigaparsec.Internal.Input where

import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty, (<|))


type InputStream :: * -> Constraint
class InputStream s where
  -- | 'True' when the input stream is empty/has reached the end
  isEmptyInputStream :: s -> Bool
  -- | Read a file and return its contents as an @s@.
  -- TODO: perhaps we should also parametrise on *where* the input comes from?
  readInputStream :: FilePath -> IO s 

  lengthInputStream :: s -> Int
  breakLinesInputStream :: Word -> NonEmpty s -> ([s], s, [s])

  dropInputStream :: Int -> s -> s

  toStringInputStream :: s -> String

  unconsInputStream :: s -> (Char, Maybe s) 

  -- nonEmptyInputStream :: s ->

type NonEmptyInputStream :: * -> Constraint
class InputStream s => NonEmptyInputStream s where
  

  -- | 'toString' is used when printing back error messages.



type Input :: *
data Input = ∀ s . InputStream s => Input !s

{-# INLINE isEmptyInput #-}
isEmptyInput :: Input -> Bool
isEmptyInput (Input x) = isEmptyInputStream x

{-# INLINE readInput #-}
readInput :: ∀ s . InputStream s => FilePath -> IO Input
readInput = (Input <$>) . readInputStream @s

{-# INLINE dropInput #-}
dropInput :: Int -> Input -> Input
dropInput n (Input x) = Input $! dropInputStream n x

{-# INLINE inputToString #-}
inputToString :: Input -> String
inputToString (Input x) = toStringInputStream x

-- Could just use Control.Arrow.second, but idk if this has the right strictness properties
{-# INLINE unconsInput #-}
unconsInput :: Input -> (Char, Maybe Input)
unconsInput (Input x) = case unconsInputStream x of
  (!c, !x') -> (c, Input <$> x')

instance InputStream String where
  {-# INLINE isEmptyInputStream #-}
  isEmptyInputStream = ([] ==)

  {-# INLINE readInputStream #-}
  readInputStream = readFile

stringInput :: String -> Input
stringInput = Input
