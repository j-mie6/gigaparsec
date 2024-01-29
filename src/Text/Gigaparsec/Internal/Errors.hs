{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards, BangPatterns, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors (
    module Text.Gigaparsec.Internal.Errors,
    CaretWidth(..), ExpectItem(..),
    Error.presentationOffset, Error.isExpectedEmpty, DefuncHints(Blank)
  ) where

import Data.Set (Set)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder)

import Text.Gigaparsec.Internal.Errors.DefuncError (DefuncError)
import Text.Gigaparsec.Internal.Errors.DefuncError qualified as Error
import Text.Gigaparsec.Internal.Errors.DefuncHints (DefuncHints(Blank))
import Text.Gigaparsec.Internal.Errors.DefuncHints qualified as Hints

import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth(FlexibleCaret, RigidCaret))
import Text.Gigaparsec.Internal.Errors.ErrorItem (ExpectItem(ExpectNamed, ExpectEndOfInput, ExpectRaw))
import Text.Gigaparsec.Internal.Errors.ParseError (fromParseError)
import Text.Gigaparsec.Internal.Errors.DefuncBuilders (asParseError)

CPP_import_PortableUnlifted

type Error :: UnliftedDatatype
type Error = DefuncError
type Hints :: UnliftedDatatype
type Hints = DefuncHints

{-# INLINE addError #-}
addError :: Hints -> Error -> Hints
addError = Hints.addError

{-# INLINE replaceHints #-}
replaceHints :: Set String -> Hints -> Hints
replaceHints = Hints.replace

{-# INLINABLE fromError #-}
fromError :: forall err. ErrorBuilder err => Maybe FilePath -> String -> Error -> err
fromError fp inp err = fromParseError fp inp (asParseError inp err)

{-# INLINE emptyErr #-}
emptyErr :: Word -> Word -> Word -> Word -> Error
emptyErr = Error.emptyError

{-# INLINE expectedErr #-}
expectedErr :: String -> Word -> Word -> Word -> Set ExpectItem -> Word -> Error
expectedErr _ = Error.expectedError

{-# INLINE specialisedErr #-}
specialisedErr :: Word -> Word -> Word -> [String] -> CaretWidth -> Error
specialisedErr = Error.specialisedError

{-# INLINE unexpectedErr #-}
unexpectedErr :: Word -> Word -> Word -> Set ExpectItem -> String -> CaretWidth -> Error
unexpectedErr = Error.unexpectedError

{-# INLINE labelErr #-}
labelErr :: Word -> Set String -> Error -> Error
labelErr = Error.label

--TODO: remove?
{-# INLINABLE explainErr #-}
explainErr :: Word -> String -> Error -> Error
explainErr !offset reason err
  | offset == Error.presentationOffset err = addReason reason err
explainErr _ _ err = err

{-# INLINE addReason #-}
addReason :: String -> Error -> Error
addReason = Error.withReason

{-# INLINE amendErr #-}
amendErr :: Word -> Word -> Word -> Error -> Error
amendErr = Error.amend False

{-# INLINE partialAmendErr #-}
partialAmendErr :: Word -> Word -> Word -> Error -> Error
partialAmendErr = Error.amend True

{-# INLINE entrenchErr #-}
entrenchErr :: Error -> Error
entrenchErr = Error.entrench

{-# INLINE dislodgeErr #-}
dislodgeErr :: Word -> Error -> Error
dislodgeErr !w = Error.dislodge (fromIntegral w) --FIXME:

{-# INLINE setLexical #-}
setLexical :: Word -> Error -> Error
setLexical = Error.markAsLexical

{-# INLINE useHints #-}
useHints :: Hints -> Error -> Error
useHints = Error.withHints

{-# INLINE mergeErr #-}
mergeErr :: Error -> Error -> Error
mergeErr = Error.merge
