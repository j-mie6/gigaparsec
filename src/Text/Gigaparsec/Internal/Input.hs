{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RoleAnnotations #-}
module Text.Gigaparsec.Internal.Input where

import Data.List (uncons)

type InputStream :: * -> *
type role InputStream _
data InputStream s = InputStream {
  isEmptyInputStream  :: !(s -> Bool),
  readInputStream     :: !(FilePath -> IO s ),
  toStringInputStream :: !(s -> String),
  unconsInputStream   :: !(s -> Maybe (Char, s))
  }

type Input :: * -> *
type role Input _
data Input s = Input s !(InputStream s)

-- import Data.Kind (Constraint)
-- import Control.Arrow (second)

-- -- TODO: add a NonEmptyInputStream subclass of InputStream
-- type InputStream :: * -> Constraint
-- class (Eq s, Show s) => InputStream s where
--   -- | 'True' when the input stream is empty/has reached the end
--   isEmptyInputStream :: s -> Bool
--   -- | Read a file and return its contents as an @s@.
--   -- TODO: perhaps we should also parametrise on *where* the input comes from?
--   readInputStream :: FilePath -> IO s 

--   toStringInputStream :: s -> String

--   unconsInputStream :: s -> Maybe (Char, s)

--   -- -- | It is best this is directly implemented
--   -- satisfyInput :: s -> Parsec Char

-- type Input :: *
-- data Input = ∀ s . InputStream s => Input !s

-- deriving stock instance Show Input

-- {-|
-- This is obviously hideously expensive.
-- -}
-- instance Eq Input where
--   (Input x) == (Input y) = toStringInputStream x == toStringInputStream y

-- {-# INLINE isEmptyInput #-}
-- isEmptyInput :: Input -> Bool
-- isEmptyInput (Input x) = isEmptyInputStream x

-- {-| Read input from a file.

-- Note: Requires a type application for the 'InputStream' type
-- -}
-- {-# INLINE readInput #-}
-- readInput :: ∀ s . InputStream s => FilePath -> IO Input
-- readInput = (Input @s <$>) . readInputStream @s


{-# INLINE inputToString #-}
inputToString :: Input s -> String
inputToString (Input x (InputStream {..})) = toStringInputStream x

-- -- Could just use Control.Arrow.second, but idk if this has the right strictness properties
-- {-# INLINABLE unconsInput #-}
-- unconsInput :: Input s -> Maybe (Char, Input s)
-- unconsInput (Input x) = (second Input) <$> unconsInputStream x
--   -- case unconsInputStream x of
--   -- Just (!c, !x') -> Just (c, Input x')
--   -- Nothing -> Nothing

-- instance InputStream String where
--   {-# INLINE isEmptyInputStream #-}
--   isEmptyInputStream = null

--   {-# INLINE readInputStream #-}
--   readInputStream = readFile

--   {-# INLINE unconsInputStream #-}
--   unconsInputStream = uncons

--   {-# INLINE toStringInputStream #-}
--   toStringInputStream = id

type StringInput :: *
type StringInput = Input String

stringInputInstance :: InputStream String 
stringInputInstance = InputStream {
    toStringInputStream = id
  , readInputStream = readFile
  , isEmptyInputStream = null
  , unconsInputStream = uncons
  }

isEmptyInput :: s -> InputStream s -> Bool
isEmptyInput x (InputStream {..}) = isEmptyInputStream x

unconsInput :: s -> InputStream s -> Maybe (Char, s)
unconsInput x (InputStream {..}) = unconsInputStream x

stringInput :: String -> Input String
stringInput s = Input s stringInputInstance
