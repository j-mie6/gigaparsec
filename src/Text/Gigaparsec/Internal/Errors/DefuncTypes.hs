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

type ErrKindSingleton :: ErrKind -> UnliftedDatatype
data ErrKindSingleton k where
  IsVanilla :: ErrKindSingleton 'Vanilla
  IsSpecialised :: ErrKindSingleton 'Specialised

type DefuncError :: UnliftedDatatype
data DefuncError = forall k. DefuncError {
    errKind :: {-# UNPACK #-} !(ErrKindSingleton k),
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
  Op :: { _underlyingOffset :: {-# UNPACK #-} !Word
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
  Merged      :: {-# UNPACK #-} !(ErrKindSingleton k)
              -> {-# UNPACK #-} !(DefuncError_ k) -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k
  AdjustCaret :: {-# UNPACK #-} !(DefuncError_ 'Specialised)
              -> {-# UNPACK #-} !(DefuncError_ 'Vanilla) -- ^ caretAdjuster
              -> ErrorOp 'Specialised
  WithHints   :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !DefuncHints -> ErrorOp 'Vanilla
  WithReason  :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !String -> ErrorOp 'Vanilla
  WithLabel   :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !(Set String) -> ErrorOp 'Vanilla
  Amended     :: {-# UNPACK #-} !(ErrKindSingleton k)
              -> {-# UNPACK #-} !Word -- ^ line
              -> {-# UNPACK #-} !Word -- ^ col
              -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k
  {-Entrenched  :: {-# UNPACK #-} !(ErrKindSingleton k)
              -> {-# UNPACK #-} !Word32 -- ^ by
              -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k
  Dislodged   :: {-# UNPACK #-} !(ErrKindSingleton k)
              -> {-# UNPACK #-} !Word32 -- ^ by
              -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k-}
  Lexical     :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> ErrorOp 'Vanilla

type DefuncHints :: UnliftedDatatype
data DefuncHints where
  Blank :: DefuncHints
  Replace :: !(Set String) -> DefuncHints
  Merge :: !DefuncHints -> !DefuncHints -> DefuncHints
  AddErr :: !DefuncHints -> {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> DefuncHints
