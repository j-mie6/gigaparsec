{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RoleAnnotations #-}
module Text.Gigaparsec.Internal.Input where

import Data.List (uncons)

{-| The operations that process an input type @s@.

This is a manual constraint record, which gives us finer control than with Haskell's constraint solver.
-}
type InputOps :: * -> *
type role InputOps _
data InputOps s = InputOps {
  -- | 'True' when the input @s@ is empty.
  isEmptyInputStream  :: !(s -> Bool),
  -- | Read the input of type @s@ from a file.
  readInputStream     :: !(FilePath -> IO s),
  -- | Convert the input into a 'String'
  toStringInputStream :: !(s -> String),
  -- | Split an input into a head 'Char' token and tail (the rest of the input). 
  -- Returns 'Nothing' if the input is empty.
  unconsInputStream   :: !(s -> Maybe (Char, s))
  }

{-| An @'Input' s@ consists of an input stream @s@ and its 'InputOps'.
-}
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

{-| Convert input to a string.

This may be expensive.
-}
{-# INLINE inputToString #-}
inputToString :: Input s -> String
inputToString (Input x (InputOps {..})) = toStringInputStream x

{-| 'True' when the input stream is empty (according to the 'InputOps').
-}
{-# INLINE isEmptyInput #-}
isEmptyInput :: s -> InputOps s -> Bool
isEmptyInput x (InputOps {..}) = isEmptyInputStream x

{-| Split an input into a head 'Char' token and tail (the rest of the input). 
Returns 'Nothing' if the input is empty.
-}
{-# INLINE unconsInput #-}
unconsInput :: s -> InputOps s -> Maybe (Char, s)
unconsInput x (InputOps {..}) = unconsInputStream x

---------------------------------------------------------------------------------------------------
-- String Input

{-| An 'Input' in which the stream is specialised to 'String'.
-}
type StringInput :: *
type StringInput = Input String

{-| 'InputOps' instance for 'String'.

Describes how to process 'String' input streams.
-}
stringInputOps :: InputOps String 
stringInputOps = InputOps {
    toStringInputStream = id
  , readInputStream = readFile
  , isEmptyInputStream = null
  , unconsInputStream = uncons
  }

{-| Treat a string @xs@ as an 'Input', using the default 'stringInputOps'
-}
{-# INLINE stringInput #-}
stringInput :: String -> Input String
stringInput s = Input s stringInputOps
