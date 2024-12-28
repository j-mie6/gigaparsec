
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedLists, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
{-|
Module      : Text.Gigaparsec.Internal.Token.Indentation
Description : This module defines some core combinators for handling indentation-sensitive grammars.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module defines some core combinators for handling indentation-sensitive grammars.

@since 0.4.0.0

-}
module Text.Gigaparsec.Internal.Token.Indentation (
  module Text.Gigaparsec.Internal.Token.Indentation
  ) where


import Prelude hiding (fail)
import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec (Parsec, eof)
import Text.Gigaparsec.Errors.Combinator (fail, (<?>))
import Text.Gigaparsec.Position (col)
import Text.Gigaparsec.Char (endOfLine)
import Text.Gigaparsec.State (make, set, get, Ref)
import Control.Applicative (many, Alternative ((<|>)))
import Control.Monad (unless, void)
import Text.Gigaparsec.Combinator.NonEmpty (some)
import Text.Gigaparsec.Combinator (skipMany)

-- | An indentation level is just the column number.
type IndentLevel = Word


{-|
Get the current indentation level of the input.

Consumes no input and always succeeds.
-}
{-# INLINE indentLevel #-}
indentLevel :: Parsec IndentLevel
indentLevel = col


{-|
An optional indent level, and whether we should be gt or eq to it.
-}
data IndentConfig =
    NewIndent
  | OldIndent  IndentLevel
  | CurrIndent IndentLevel

{-|
Convert a maybe indent level @mlvl@ to a config.
If @mlvl@ is @`Just` lvl@, then the resulting config is an @`OldIndent` lvl@.
-}
{-# INLINE fromOldIndent #-}
fromOldIndent :: Maybe IndentLevel -> IndentConfig
fromOldIndent Nothing = NewIndent
fromOldIndent (Just x) = OldIndent x

{-# INLINE toIndentLevel #-}
toIndentLevel :: IndentConfig -> Maybe IndentLevel
toIndentLevel NewIndent = Nothing
toIndentLevel (OldIndent x) = Just x
toIndentLevel (CurrIndent x) = Just x

{-|
An indentation error.
-}
data ErrorIndentation =
  -- | A non-indented item was actually indented.
    ErrNonIndented
      IndentLevel -- ^ Actual level
  |
  -- | The indentation of the current item did not compare with the reference level as the given ordering.
    ErrIndentNotOrd
      Ordering    -- ^ How the actual level should be compared with the reference level.
      IndentLevel -- ^ Reference level
      IndentLevel -- ^ Actual level

{-# COMPLETE ErrNonIndented, ErrIndentNotEQ, ErrIndentNotGT, ErrIndentNotLT #-}

-- | The indentation of the current item did not match that of the previous items.
pattern ErrIndentNotEQ
  :: IndentLevel -- ^ Reference level
  -> IndentLevel -- ^ Actual level 
  -> ErrorIndentation
pattern ErrIndentNotEQ ref act = ErrIndentNotOrd EQ ref act
-- | The new indentation block was not more indented than the reference token.
pattern ErrIndentNotGT
  :: IndentLevel -- ^ Reference level
  -> IndentLevel -- ^ Actual level 
  -> ErrorIndentation
pattern ErrIndentNotGT ref act = ErrIndentNotOrd GT ref act

-- | The new indentation block was not less indented than the reference token.
pattern ErrIndentNotLT
  :: IndentLevel -- ^ Reference level
  -> IndentLevel -- ^ Actual level 
  -> ErrorIndentation
pattern ErrIndentNotLT ref act = ErrIndentNotOrd LT ref act

{-|
Throws an `ErrorIndentation` as a specialised error.
-}
{-# INLINABLE throwIndentationError #-}
throwIndentationError :: ErrorIndentation -> Parsec a
throwIndentationError e = case e of
  ErrNonIndented act     -> err ["Item should not be indented. Actual indentation: ", show act]
  ErrIndentNotEQ exp act -> err ["Indentation changed unexpectedly. Expected: ", show exp, ", actual: ", show act]
  ErrIndentNotGT exp act -> err ["Indentation block is not more indented than the reference token. Reference indentation: ", show exp, ", indentation of the block: ", show act]
  ErrIndentNotLT exp act -> err ["Indentation block is not less indented than the reference token. Reference indentation: ", show exp, ", indentation of the block: ", show act]
  where
    err s = fail [concat ("Indentation Error: ": s)]

{-|
Parse a non-indented item, @p@.

This ensures that there is no indentation before @p@; if there is, this combinator fails.
This combinator consumes input if there is any whitespace before @p@ and/or if @p@ does also.

@since 0.4.0.0
-}
{-# INLINE nonIndented #-}
nonIndented
  :: Parsec () -- ^ Whitespace consumer
  -> Parsec a  -- ^ @p@, the parser to run
  -> Parsec a  -- ^ Parses @p@ only if it is not indented, otherwise fails.
nonIndented ws p = do
  lvl <- ws *> indentLevel
  unless (lvl == 1) $ throwIndentationError (ErrNonIndented lvl)
  p

{-|

@since 0.4.0.0

-}
{-# INLINE indentMany #-}
indentMany
  :: forall b
  .  Maybe IndentLevel -- ^ reference indentation level
  -> Parsec () -- ^ whitespace consumer, does not need to consume newlines
  -> Parsec b  -- ^ the indented items to parse
  -> Parsec [b]
indentMany mlvl ws p = indentCommon mlvl ws p many


{-|

@since 0.4.0.0

-}
{-# INLINE indentSome #-}
indentSome
  :: forall b
  .  Maybe IndentLevel -- ^ reference indentation level
  -> Parsec () -- ^ whitespace consumer, does not need to consume newlines
  -> Parsec b  -- ^ the indented items to parse
  -> Parsec (NonEmpty b)
indentSome mlvl ws p = indentCommon mlvl ws p some

{-|
Captures the common functionality between indentSome and indentMany.

I guess you could set @q@ to something other than `some` or `many`,
although I'm not sure what sort of wacky parser you'd get from this.

@since 0.4.0.0
-}
-- Doesn't do atomic, does consume last newline
{-# INLINE indentCommon #-}
indentCommon
  :: forall a b
  .  Maybe IndentLevel -- ^ reference indentation level
  -> Parsec () -- ^ whitespace consumer, must consume newlines
  -> Parsec a  -- ^ the indented items to parse
  -> (Parsec a -> Parsec b) -- ^ @q@, how to collect each item
  -> Parsec b
indentCommon mlvl ws p q =
  make (fromOldIndent mlvl) $ \indentRef -> do
    endOfInputOrLine -- optional endOfLine *> ws
    q (indentedItem indentRef)
  where
    {-|
    Checks indentation level first and fails (w/o consuming input) if it is incorrect. 
    -}
    {-# INLINE indentedItem #-}
    indentedItem :: Ref r IndentConfig -> Parsec a
    indentedItem indentRef = do
      checkIndentLvl indentRef
      p <* endOfInputOrLine

    {-# INLINE endOfInputOrLine #-}
    endOfInputOrLine :: Parsec ()
    endOfInputOrLine =
      (eof <|> ws)
        <?> ["end of indentation block"]

    {-| 
      It is essential that this does not consume input.
      Then, if it is incorrectly indented, `indentedItem` will fail without consuming input,
      this lets us exit `some` and `many` (called `q` here) without failing
      thereby keeping previously parsed results.
    -}
    {-# INLINABLE checkIndentLvl #-}
    checkIndentLvl ref = do
      -- do we need to check `eof` here?
      !newIndentLvl <- indentLevel
      !mExpectedIndentLvl <- get ref
      case mExpectedIndentLvl of
        NewIndent
          -> set ref (CurrIndent newIndentLvl)
        OldIndent  expectedIndentLvl
          | expectedIndentLvl < newIndentLvl -> set ref (CurrIndent newIndentLvl)
          | otherwise -> throwIndentationError (ErrIndentNotGT expectedIndentLvl newIndentLvl)
        CurrIndent expectedIndentLvl
          | expectedIndentLvl == newIndentLvl -> pure ()
          | otherwise -> throwIndentationError (ErrIndentNotEQ expectedIndentLvl newIndentLvl)
