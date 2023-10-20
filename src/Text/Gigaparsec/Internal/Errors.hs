{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Errors where

data CaretWidth = FlexibleCaret { width :: {-# UNPACK #-} !Int }
                | RigidCaret { width :: {-# UNPACK #-} !Int }

isFlexible :: CaretWidth -> Bool
isFlexible FlexibleCaret{} = True
isFlexible _               = False
