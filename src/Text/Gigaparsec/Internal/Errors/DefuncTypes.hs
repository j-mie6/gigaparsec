{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, DataKinds, GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
-- Yes, this is redundant, however, it is necessary to get the UNPACK to fire
{-# OPTIONS_GHC -Wno-redundant-strictness-flags #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors.DefuncTypes (
    module Text.Gigaparsec.Internal.Errors.DefuncTypes
  ) where

import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth (width), Span)
import Text.Gigaparsec.Internal.Errors.ErrorItem (ExpectItem)

import Data.Word (Word32)
import Data.Set (Set)
import Data.Set qualified as Set (empty)

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
    underlyingOffset :: {-# UNPACK #-} !Word,
    errTy :: {-# UNPACK #-} !(DefuncError_ k)
  }

type DefuncError_ :: ErrKind -> UnliftedDatatype
data DefuncError_ k where
  Base :: {-# UNPACK #-} !Word -- ^ line
       -> {-# UNPACK #-} !Word -- ^ col
       -> {-# UNPACK #-} !(BaseError k)
       -> DefuncError_ k
  Op :: {-# UNPACK #-} !(ErrorOp k) -> DefuncError_ k


type BaseError :: ErrKind -> UnliftedDatatype
data BaseError k where
  ClassicSpecialised :: ![String] -> {-# UNPACK #-} !CaretWidth -> BaseError 'Specialised
  Expected :: !(Set ExpectItem) -> {-# UNPACK #-} !Span -> BaseError 'Vanilla
  Unexpected :: !(Set ExpectItem) -> !String -> {-# UNPACK #-} !CaretWidth -> BaseError 'Vanilla
  Empty :: {-# UNPACK #-} !Span -> BaseError 'Vanilla

{-# INLINABLE expecteds #-}
expecteds :: BaseError 'Vanilla -> Set ExpectItem
expecteds (Expected exs _) = exs
expecteds (Unexpected exs _ _) = exs
expecteds Empty{} = Set.empty

{-# INLINEABLE unexpectedWidth #-}
unexpectedWidth :: BaseError 'Vanilla -> Word
unexpectedWidth (Expected _ w) = w
unexpectedWidth (Unexpected _ _ cw) = width cw
unexpectedWidth (Empty w) = w


type ErrorOp :: ErrKind -> UnliftedDatatype
data ErrorOp k where
  Merged      :: {-# UNPACK #-} !(DefuncError_ k) -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k
  AdjustCaret :: {-# UNPACK #-} !(DefuncError_ 'Specialised)
              -> {-# UNPACK #-} !(DefuncError_ 'Vanilla) -- ^ caretAdjuster
              -> ErrorOp 'Specialised
  WithHints   :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !DefuncHints -> ErrorOp 'Vanilla
  WithReason  :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !String -> ErrorOp 'Vanilla
  WithLabel   :: {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> !(Set String) -> ErrorOp 'Vanilla
  Amended     :: {-# UNPACK #-} !Word -- ^ line
              -> {-# UNPACK #-} !Word -- ^ col
              -> {-# UNPACK #-} !(DefuncError_ k) -> ErrorOp k

type DefuncHints :: UnliftedDatatype
data DefuncHints where
  Blank :: DefuncHints
  Replace :: !(Set String) -> DefuncHints
  Merge :: !DefuncHints -> !DefuncHints -> DefuncHints
  AddErr :: !DefuncHints -> {-# UNPACK #-} !(DefuncError_ 'Vanilla) -> DefuncHints
