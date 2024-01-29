{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors.CaretControl (
    module Text.Gigaparsec.Internal.Errors.CaretControl
  ) where

CPP_import_PortableUnlifted

type Span :: *
type Span = Word

type CaretWidth :: UnliftedDatatype
data CaretWidth = FlexibleCaret { width :: {-# UNPACK #-} !Span }
                | RigidCaret { width :: {-# UNPACK #-} !Span }

{-# INLINE isFlexible #-}
isFlexible :: CaretWidth -> Bool
isFlexible FlexibleCaret{} = True
isFlexible _               = False
