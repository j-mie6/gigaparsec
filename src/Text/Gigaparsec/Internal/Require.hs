{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Internal.Require (require) where

import GHC.Stack (HasCallStack)

require :: HasCallStack => Bool -> String -> a -> a
require True _ = id
require False msg = error ("Requirement unsatisfied: " ++ msg)
