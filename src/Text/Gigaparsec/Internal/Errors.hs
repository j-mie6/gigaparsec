{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards, BangPatterns, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors (
    module Text.Gigaparsec.Internal.Errors,
    CaretWidth(..), ExpectItem(..),
    Error.presentationOffset, Error.isExpectedEmpty
  ) where

import Data.Set (Set)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder)

import Text.Gigaparsec.Internal.Errors.DefuncError (DefuncError)
import Text.Gigaparsec.Internal.Errors.DefuncError qualified as Error
import Text.Gigaparsec.Internal.Errors.DefuncHints (DefuncHints)
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

emptyHints :: () -> Hints
emptyHints = Hints.empty

addError :: Hints -> Error -> Hints
addError = Hints.addError

replaceHints :: Set String -> Hints -> Hints
replaceHints = Hints.replace

{-# INLINABLE fromError #-}
fromError :: forall err. ErrorBuilder err => Maybe FilePath -> String -> Error -> err
fromError fp inp err = fromParseError fp inp (asParseError inp err)

emptyErr :: Word -> Word -> Word -> Word -> Error
emptyErr = Error.emptyError

expectedErr :: String -> Word -> Word -> Word -> Set ExpectItem -> Word -> Error
expectedErr _ = Error.expectedError

specialisedErr :: Word -> Word -> Word -> [String] -> CaretWidth -> Error
specialisedErr = Error.specialisedError

unexpectedErr :: Word -> Word -> Word -> Set ExpectItem -> String -> CaretWidth -> Error
unexpectedErr = Error.unexpectedError

labelErr :: Word -> Set String -> Error -> Error
labelErr = Error.label

--TODO: remove?
explainErr :: Word -> String -> Error -> Error
explainErr !offset reason err
  | offset == Error.presentationOffset err = addReason reason err
explainErr _ _ err = err

addReason :: String -> Error -> Error
addReason = Error.withReason

amendErr :: Word -> Word -> Word -> Error -> Error
amendErr = Error.amend False

partialAmendErr :: Word -> Word -> Word -> Error -> Error
partialAmendErr = Error.amend True

entrenchErr :: Error -> Error
entrenchErr = Error.entrench

dislodgeErr :: Word -> Error -> Error
dislodgeErr !w = Error.dislodge (fromIntegral w) --FIXME:

setLexical :: Word -> Error -> Error
setLexical = Error.markAsLexical

useHints :: Hints -> Error -> Error
useHints = Error.withHints

mergeErr :: Error -> Error -> Error
mergeErr = Error.merge
