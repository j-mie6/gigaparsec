{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, DataKinds, GADTs #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-all-missed-specialisations -Wno-missing-import-lists #-}
{-# OPTIONS_HADDOCK hide #-}
-- Yes, this is redundant, however, it is necessary to get the UNPACK to fire
{-# OPTIONS_GHC -Wno-redundant-strictness-flags #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors.DefuncTypes (
    module Text.Gigaparsec.Internal.Errors.DefuncTypes
  ) where

import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth, Span)
import Text.Gigaparsec.Internal.Errors.ErrorItem (ExpectItem)

import Data.Word (Word32)
import Data.Set (Set)

CPP_import_PortableUnlifted

type ErrKind :: *
data ErrKind = Vanilla | Specialised

type DefuncError :: ErrKind -> UnliftedDatatype
data DefuncError k = DefuncError {
    flags :: {-# UNPACK #-} !Word32,
    presentationOffset :: {-# UNPACK #-} !Word,
    errTy :: {-# UNPACK #-} !(DefuncError_ k)
  }

type DefuncError_ :: ErrKind -> UnliftedDatatype
data DefuncError_ k where
  Base :: { line :: {-# UNPACK #-} !Word
          , col :: {-# UNPACK #-} !Word
          , base :: {-# UNPACK #-} !(BaseError k)
          }
       -> DefuncError_ k
  Op :: { underlyingOffset :: {-# UNPACK #-} !Word
        , op :: {-# UNPACK #-} !(ErrorOp k)
        } -> DefuncError_ k


type BaseError :: ErrKind -> UnliftedDatatype
data BaseError k where
  ClassicSpecialised :: ![String] -> BaseError 'Specialised
  Expected :: !(Set ExpectItem) -> {-# UNPACK #-} !Span -> BaseError 'Vanilla
  ExpectedWithReason :: !(Set ExpectItem) -> !String -> {-# UNPACK #-} !Span -> BaseError 'Vanilla
  Unexpected :: !(Set ExpectItem) -> !String -> {-# UNPACK #-} !CaretWidth -> BaseError 'Vanilla
  Empty :: {-# UNPACK #-} !Span -> BaseError 'Vanilla
  EmptyWithReason :: !String -> {-# UNPACK #-} !Span -> BaseError 'Vanilla

type ErrorOp :: ErrKind -> UnliftedDatatype
data ErrorOp k where
  Merged      :: !(DefuncError k) -> !(DefuncError k) -> ErrorOp k
  AdjustCaret :: !(DefuncError 'Specialised)
              -> !(DefuncError 'Vanilla) -- ^ caretAdjuster
              -> ErrorOp 'Specialised
  WithHints   :: !(DefuncError 'Vanilla) -> !DefuncHints -> ErrorOp 'Vanilla
  WithReason  :: !(DefuncError 'Vanilla) -> !String -> ErrorOp 'Vanilla
  WithLabel   :: !(DefuncError 'Vanilla) -> !(Set String) -> ErrorOp 'Vanilla
  Amended     :: {-# UNPACK #-} !Word -- ^ line
              -> {-# UNPACK #-} !Word -- ^ col
              -> !(DefuncError k) -> ErrorOp k
  Entrenched  :: {-# UNPACK #-} !Word -- ^ by
              -> !(DefuncError k) -> ErrorOp k
  Dislodged   :: {-# UNPACK #-} !Word -- ^ by
              -> !(DefuncError k) -> ErrorOp k
  Lexical     :: !(DefuncError 'Vanilla) -> ErrorOp 'Vanilla

type DefuncHints :: UnliftedDatatype
data DefuncHints where
  Blank :: DefuncHints
  Replace :: !(Set String) -> DefuncHints
  Merge :: !DefuncHints -> !DefuncHints -> DefuncHints
  AddErr :: !DefuncHints -> !(DefuncError 'Vanilla) -> DefuncHints
