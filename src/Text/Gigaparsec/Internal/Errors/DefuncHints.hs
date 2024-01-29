{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, NamedFieldPuns, BinaryLiterals, NumericUnderscores, DataKinds, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Internal.Errors.DefuncHints (
    DefuncHints(Blank),
    replace, addError
  ) where

import Text.Gigaparsec.Internal.Errors.DefuncTypes (
    DefuncHints(Blank, Replace, AddErr),
    DefuncError(DefuncError), ErrKindSingleton(IsVanilla)
  )

import Data.Set (Set)

{-# INLINABLE replace #-}
replace :: Set String -> DefuncHints -> DefuncHints
replace !_ Blank = Blank
replace ls _      = Replace ls

{-# INLINABLE addError #-}
addError :: DefuncHints -> DefuncError -> DefuncHints
addError hints (DefuncError IsVanilla _ _ _ err) = AddErr hints err
addError _ _ = error "invariance broken: a specialised error is never added to hints"
