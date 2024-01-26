{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, NamedFieldPuns, BinaryLiterals, NumericUnderscores, DataKinds, BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Internal.Errors.DefuncError (
    DefuncError,
    merge, withHints, withReason, withReasonAndOffset, label,
    amend, entrench, dislodge, markAsLexical,
    isVanilla, isExpectedEmpty, isLexical
  ) where

import Data.Word (Word32)
import Data.Bits ((.&.), testBit, clearBit, setBit, complement)
import Data.Set (Set)
import Data.Set qualified as Set (null)

import Text.Gigaparsec.Internal.Errors.DefuncTypes (
    DefuncError(..), DefuncError_(..), ErrKindSingleton(..), ErrorOp(..),
    DefuncHints(..)
  )

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

merge :: DefuncError -> DefuncError -> DefuncError
merge err1 err2 = case compare (underlyingOffset err1) (underlyingOffset err2) of
  GT -> err1
  LT -> err2
  EQ -> case compare (presentationOffset err1) (presentationOffset err2) of
    GT -> err1
    LT -> err2
    EQ -> case err1 of
      DefuncError IsSpecialised flags1 pOff uOff errTy1
        | DefuncError IsSpecialised flags2 _ _ errTy2 <- err2 ->
            DefuncError IsSpecialised (flags1 .&. flags2) pOff uOff (Op (Merged errTy1 errTy2))
        | DefuncError IsVanilla _ _ _ errTy2 <- err2
        , isFlexibleCaret err1                    ->
            DefuncError IsSpecialised flags1 pOff uOff (Op (AdjustCaret errTy1 errTy2))
        | otherwise                               -> err1
      DefuncError IsVanilla flags1 pOff uOff errTy1
        | DefuncError IsVanilla flags2 _ _ errTy2 <- err2     ->
            DefuncError IsVanilla (flags1 .&. flags2) pOff uOff (Op (Merged errTy1 errTy2))
        | DefuncError IsSpecialised _ _ _ errTy2 <- err2
        , isFlexibleCaret err2                    ->
            DefuncError IsSpecialised flags1 pOff uOff (Op (AdjustCaret errTy2 errTy1))
        | otherwise                               -> err2

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

label :: Set String -> Word -> DefuncError -> DefuncError
label !labels !off (DefuncError IsVanilla flags pOff uOff errTy) | pOff == off =
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
markAsLexical !off (DefuncError IsVanilla flags pOff uOff errTy) | off == pOff =
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
