{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RoleAnnotations #-}
module Text.Gigaparsec.Internal.Input where

import Data.List (uncons)

type InputOps :: * -> *
type role InputOps _
data InputOps s = InputOps {
  isEmptyInputStream  :: !(s -> Bool),
  
  readInputStream     :: !(FilePath -> IO s),
  toStringInputStream :: !(s -> String),
  unconsInputStream   :: !(s -> Maybe (Char, s))
  }

type Input :: * -> *
type role Input _
data Input s = Input !s !(InputOps s)
  
instance Eq s => Eq (Input s) where
  (Input x _) == (Input y _) = x == y

instance Show s => Show (Input s) where
  show (Input x _) = show x

{-| Read input from a file.

Note: Requires a type application for the 'InputOps' type
-}
{-# INLINE readInput #-}
readInput :: InputOps s -> FilePath -> IO (Input s)
readInput ops = ((`Input` ops) <$>) . (readInputStream ops) 


{-# INLINE inputToString #-}
inputToString :: Input s -> String
inputToString (Input x (InputOps {..})) = toStringInputStream x

type StringInput :: *
type StringInput = Input String

stringInputInstance :: InputOps String 
stringInputInstance = InputOps {
    toStringInputStream = id
  , readInputStream = readFile
  , isEmptyInputStream = null
  , unconsInputStream = uncons
  }

{-# INLINE isEmptyInput #-}
isEmptyInput :: s -> InputOps s -> Bool
isEmptyInput x (InputOps {..}) = isEmptyInputStream x

{-# INLINE unconsInput #-}
unconsInput :: s -> InputOps s -> Maybe (Char, s)
unconsInput x (InputOps {..}) = unconsInputStream x

{-# INLINE stringInput #-}
stringInput :: String -> Input String
stringInput s = Input s stringInputInstance
