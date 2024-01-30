{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, NamedFieldPuns, BinaryLiterals, NumericUnderscores, DataKinds, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Internal.Errors.DefuncError (
    DefuncError(presentationOffset),
    specialisedError, expectedError, unexpectedError, emptyError,
    merge, withHints, withReason, withReasonAndOffset, label,
    amend, entrench, dislodge, markAsLexical,
    isVanilla, isExpectedEmpty, isLexical
  ) where

import Data.Word (Word32)
import Data.Bits ((.&.), (.|.), testBit, clearBit, setBit, complement, bit)
import Data.Set (Set)
import Data.Set qualified as Set (null)

import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth, Span, isFlexible)
import Text.Gigaparsec.Internal.Errors.DefuncTypes (
    DefuncError(..), DefuncError_(..), ErrKindSingleton(..),
    ErrorOp(..), BaseError(..),
    DefuncHints(..)
  )
import Text.Gigaparsec.Internal.Errors.ErrorItem (ExpectItem)

{-# INLINABLE isVanilla #-}
isVanilla :: DefuncError -> Bool
isVanilla DefuncError{flags} = testBit flags vanillaBit

{-# INLINABLE isExpectedEmpty #-}
isExpectedEmpty :: DefuncError -> Bool
isExpectedEmpty DefuncError{flags} = testBit flags expectedEmptyBit

{-# INLINABLE entrenchedBy #-}
entrenchedBy :: DefuncError -> Word32
entrenchedBy DefuncError{flags} = flags .&. entrenchedMask

{-# INLINABLE entrenched #-}
entrenched :: DefuncError -> Bool
entrenched err = entrenchedBy err > 0

{-# INLINABLE isFlexibleCaret #-}
isFlexibleCaret :: DefuncError -> Bool
isFlexibleCaret DefuncError{flags} = testBit flags flexibleCaretBit

{-# INLINABLE isLexical #-}
isLexical :: DefuncError -> Bool
isLexical DefuncError{flags} = testBit flags lexicalBit

-- Base Errors
specialisedError :: Word -> Word -> Word -> [String] -> CaretWidth -> DefuncError
specialisedError !pOff !line !col !msgs caret =
  DefuncError IsSpecialised flags pOff pOff (Base line col (ClassicSpecialised msgs caret))
  where flags :: Word32
        !flags
          | isFlexible caret = setBit (bit expectedEmptyBit) flexibleCaretBit
          | otherwise        = bit expectedEmptyBit

emptyError :: Word -> Word -> Word -> Span -> DefuncError
emptyError !pOff !line !col !unexWidth =
  DefuncError IsVanilla flags pOff pOff (Base line col (Empty unexWidth))
  where flags :: Word32
        !flags = setBit (bit vanillaBit) expectedEmptyBit

expectedError :: Word -> Word -> Word -> Set ExpectItem -> Span -> DefuncError
expectedError !pOff !line !col !exs !unexWidth =
  DefuncError IsVanilla flags pOff pOff (Base line col (Expected exs unexWidth))
  where flags :: Word32
        !flags
          | Set.null exs = setBit (bit vanillaBit) expectedEmptyBit
          | otherwise    = bit vanillaBit

unexpectedError :: Word -> Word -> Word -> Set ExpectItem -> String -> CaretWidth -> DefuncError
unexpectedError !pOff !line !col !exs !unex caretWidth =
  DefuncError IsVanilla flags pOff pOff (Base line col (Unexpected exs unex caretWidth))
  where flags :: Word32
        !flags
          | Set.null exs = setBit (bit vanillaBit) expectedEmptyBit
          | otherwise    = bit vanillaBit

-- Operations
merge :: DefuncError -> DefuncError -> DefuncError
merge err1@(DefuncError k1 flags1 pOff1 uOff1 errTy1)
      err2@(DefuncError k2 flags2 pOff2 uOff2 errTy2) =
  case compare uOff1 uOff2 of
    GT -> err1
    LT -> err2
    EQ -> case compare pOff1 pOff2 of
      GT -> err1
      LT -> err2
      EQ -> case k1 of
        IsSpecialised -> case k2 of
          IsSpecialised ->
            DefuncError IsSpecialised (combineFlags flags1 flags2) pOff1 uOff1 (Op (Merged errTy1 errTy2))
          IsVanilla | isFlexibleCaret err1 ->
            DefuncError IsSpecialised flags1 pOff1 uOff1 (Op (AdjustCaret errTy1 errTy2))
          _ -> err1
        IsVanilla -> case k2 of
          IsVanilla ->
            DefuncError IsVanilla (combineFlags flags1 flags2) pOff1 uOff1 (Op (Merged errTy1 errTy2))
          IsSpecialised | isFlexibleCaret err2 ->
            DefuncError IsSpecialised flags1 pOff1 uOff1 (Op (AdjustCaret errTy2 errTy1))
          _ -> err2
  where combineFlags f1 f2 =
          (f1 .&. f2 .&. complement entrenchedMask) .|. max (f1 .&. entrenchedMask) (f2 .&. entrenchedMask)

withHints :: DefuncHints -> DefuncError -> DefuncError
withHints Blank err = err
withHints hints (DefuncError IsVanilla flags pOff uOff errTy) =
  DefuncError IsVanilla (clearBit flags expectedEmptyBit) pOff uOff (Op (WithHints errTy hints))
withHints _ err = err

withReasonAndOffset :: String -> Word -> DefuncError -> DefuncError
withReasonAndOffset !reason !off (DefuncError IsVanilla flags pOff uOff errTy) | pOff == off =
  DefuncError IsVanilla flags pOff uOff (Op (WithReason errTy reason))
withReasonAndOffset _ _ err = err

withReason :: String -> DefuncError -> DefuncError
withReason !reason err = withReasonAndOffset reason (presentationOffset err) err

label :: Word -> Set String -> DefuncError -> DefuncError
label !off !labels (DefuncError IsVanilla flags pOff uOff errTy) | pOff == off =
  DefuncError IsVanilla flags' pOff uOff (Op (WithLabel errTy labels))
  where !flags'
          | Set.null labels = setBit flags expectedEmptyBit
          | otherwise       = clearBit flags expectedEmptyBit
label _ _ err = err

amend :: Bool -> Word -> Word -> Word -> DefuncError -> DefuncError
amend !partial !pOff !line !col err@(DefuncError k flags _ uOff errTy)
  | entrenched err = err
  | otherwise = DefuncError k flags pOff uOff' (Op (Amended line col errTy))
  where
    !uOff' = if partial then uOff else pOff

entrench :: DefuncError -> DefuncError
entrench (DefuncError k flags pOff uOff errTy) = DefuncError k (flags + 1) pOff uOff errTy

dislodge :: Word32 -> DefuncError -> DefuncError
dislodge by err@(DefuncError k flags pOff uOff errTy)
  | eBy == 0  = err
  | eBy > by  = DefuncError k (flags - by) pOff uOff errTy
  | otherwise = DefuncError k (flags .&. complement entrenchedMask) pOff uOff errTy
  where !eBy = entrenchedBy err

markAsLexical :: Word -> DefuncError -> DefuncError
markAsLexical !off (DefuncError IsVanilla flags pOff uOff errTy) | off < pOff =
  DefuncError IsVanilla (setBit flags lexicalBit) pOff uOff errTy
markAsLexical _ err = err

-- FLAG MASKS
{-# INLINE vanillaBit #-}
{-# INLINE expectedEmptyBit #-}
{-# INLINE lexicalBit #-}
{-# INLINE flexibleCaretBit #-}
{-# INLINE entrenchedMask #-}
vanillaBit, expectedEmptyBit, lexicalBit, flexibleCaretBit :: Int
vanillaBit       = 31
expectedEmptyBit = 30
lexicalBit       = 29
flexibleCaretBit = 28
entrenchedMask :: Word32
entrenchedMask    = 0b00001111_11111111_11111111_11111111
