{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs, DataKinds, UnboxedTuples, UnboxedSums, PatternSynonyms, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- Yes, this is redundant, however, it is necessary to get the UNPACK to fire
{-# OPTIONS_GHC -Wno-redundant-strictness-flags #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors.DefuncBuilders (
    asParseError
  ) where

import Text.Gigaparsec.Internal.Errors.DefuncTypes (
    DefuncHints(Blank, Merge, AddErr, Replace),
    ErrorOp(Amended, WithLabel, WithHints, Merged, WithReason),
    BaseError(Unexpected, Empty, Expected),
    DefuncError_(Op, Base),
    DefuncError(DefuncError, presentationOffset, errKind, errTy),
    ErrKindSingleton(IsSpecialised, IsVanilla),
    ErrKind(Vanilla),
    expecteds, unexpectedWidth
  )
import Text.Gigaparsec.Internal.Errors (ParseError(VanillaError, SpecialisedError), CaretWidth)
import Text.Gigaparsec.Internal.Errors.DefuncError (isLexical)
import Text.Gigaparsec.Internal.Errors.ErrorItem (
    ExpectItem(ExpectNamed),
    UnexpectItem(UnexpectEndOfInput, UnexpectNamed, UnexpectRaw)
  )

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty (nonEmpty)

CPP_import_PortableUnlifted
import GHC.Exts (TYPE, RuntimeRep(SumRep), LiftedRep, ZeroBitRep)

asParseError :: String -> DefuncError -> ParseError
asParseError !input e@DefuncError{..} = case errKind of
  IsVanilla -> case makeVanilla 0 0 Set.empty (NoItem 0) Set.empty True errTy of
    (# line, col, exs, unex, reasons #) ->
      VanillaError presentationOffset line col (toErrorItem input presentationOffset unex) exs reasons (isLexical e) 0 0
  IsSpecialised -> undefined
  where
    !outOfRange = presentationOffset < fromIntegral (length input)

    makeVanilla :: Word -> Word -> Set ExpectItem -> BuilderUnexpectItem -> Set String -> Bool
                -> DefuncError_ 'Vanilla
                -> (# Word, Word, Set ExpectItem, BuilderUnexpectItem, Set String #)
    makeVanilla !_ !_ !exs unex !reasons !acceptingExpected (Base line col err) =
      case err of
        Empty unexWidth ->
          (# line, col, exs, updateEmptyUnexpected unexWidth unex, reasons #)
        Expected exs' unexWidth ->
          (# line, col, addLabels acceptingExpected exs exs', updateUnexpected outOfRange unexWidth unex, reasons #)
        Unexpected exs' unex' caretWidth ->
          (# line, col, addLabels acceptingExpected exs exs', updateUnexpected' unex' caretWidth unex, reasons #)
    makeVanilla line col exs unex reasons acceptingExpected (Op op) =
      case op of
        Merged err1 err2 ->
          case makeVanilla line col exs unex reasons acceptingExpected err1 of
            (# line', col', exs', unex', reasons' #) ->
              makeVanilla line' col' exs' unex' reasons' acceptingExpected err2
        WithHints err hints ->
          case makeVanilla line col exs unex reasons acceptingExpected err of
            (# line', col', exs', unex', reasons' #) ->
              if acceptingExpected then
                case collectHints exs' UNothing hints of
                  (# exs'', UJust width #) ->
                    (# line', col', exs'', updateUnexpected outOfRange width unex', reasons' #)
                  (# exs'', UNothing #) -> (# line', col', exs'', unex', reasons' #)
              else (# line', col', exs', unex', reasons' #)
        WithLabel err ls ->
          case makeVanilla line col exs unex reasons False err of
            (# line', col', exs', unex', reasons' #) ->
              (# line', col', addLabels acceptingExpected exs' (Set.map ExpectNamed ls), unex', reasons' #)
        WithReason err reason ->
          makeVanilla line col exs unex (Set.insert reason reasons) acceptingExpected err
        Amended line' col' err ->
          case makeVanilla line col exs unex reasons acceptingExpected err of
            (# _, _, exs', unex', reasons' #) ->
              (# line', col', exs', unex', reasons' #)

-- FIXME: unlifted
type BuilderUnexpectItem :: UnliftedDatatype
data BuilderUnexpectItem = NoItem {-# UNPACK #-} !Word
                         | RawItem {-# UNPACK #-} !Word
                         | NamedItem !String {-# UNPACK #-} !CaretWidth
                         | EndOfInput

updateEmptyUnexpected :: Word -> BuilderUnexpectItem -> BuilderUnexpectItem
updateEmptyUnexpected !w = pickHigher (NoItem w)

updateUnexpected :: Bool -> Word -> BuilderUnexpectItem -> BuilderUnexpectItem
updateUnexpected !outOfRange !w
 | outOfRange = pickHigher EndOfInput
 | otherwise  = pickHigher (RawItem w)

updateUnexpected' :: String -> CaretWidth -> BuilderUnexpectItem -> BuilderUnexpectItem
updateUnexpected' item cw = pickHigher (NamedItem item cw)

pickHigher :: BuilderUnexpectItem -> BuilderUnexpectItem -> BuilderUnexpectItem
pickHigher _ _ = undefined

addLabels :: Bool -> Set ExpectItem -> Set ExpectItem -> Set ExpectItem
addLabels True !exs !exs' = Set.union exs exs'
addLabels False exs _     = exs

toErrorItem :: String -> Word -> BuilderUnexpectItem -> Either Word UnexpectItem
toErrorItem !_ !_ (NoItem w) = Left w
toErrorItem _ _ (NamedItem item cw) = Right (UnexpectNamed item cw)
toErrorItem _ _ EndOfInput = Right UnexpectEndOfInput
toErrorItem input off (RawItem w) =
  case nonEmpty (drop (fromIntegral off) input) of
    Nothing -> Right UnexpectEndOfInput
    Just cs -> Right (UnexpectRaw cs w)

type UMaybe :: * -> TYPE ('SumRep '[ZeroBitRep, LiftedRep])
type UMaybe a = (# (# #) | a #)
{-# COMPLETE UJust, UNothing #-}
pattern UJust :: a -> UMaybe a
pattern UJust x = (# | x #)
pattern UNothing :: UMaybe a
pattern UNothing = (# (# #) | #)

collectHints :: Set ExpectItem -> UMaybe Word -> DefuncHints -> (# Set ExpectItem, UMaybe Word #)
collectHints !exs width Blank = (# exs, width #)
collectHints exs width (Replace ls) = (# Set.union exs (Set.map ExpectNamed ls), width #)
collectHints exs width (Merge hints1 hints2) =
  let !(# exs', width' #) = collectHints exs width hints1
  in  collectHints exs' width' hints2
collectHints exs width (AddErr hints err) =
  let !(# exs', width' #) = collectHintsErr exs width err
  in collectHints exs' width' hints

collectHintsErr :: Set ExpectItem -> UMaybe Word -> DefuncError_ 'Vanilla -> (# Set ExpectItem, UMaybe Word #)
collectHintsErr !exs width (Base _ _ err) =
  (# Set.union exs (expecteds err), updateWidth width (unexpectedWidth err) #)
collectHintsErr exs width (Op op) = case op of
  -- FIXME: Why doesn't this traverse deeper to collect the width?
  WithLabel _ ls -> (# Set.union exs (Set.map ExpectNamed ls), width #)
  WithHints err hints ->
    let !(# exs', width' #) = collectHints exs width hints
    in collectHintsErr exs' width' err
  Merged err1 err2 ->
    let !(# exs', width' #) = collectHintsErr exs width err1
    in collectHintsErr exs' width' err2
  WithReason err _ -> collectHintsErr exs width err
  Amended _ _ err -> collectHintsErr exs width err

updateWidth :: UMaybe Word -> Word -> UMaybe Word
updateWidth UNothing !w = UJust w
updateWidth (UJust w) w' = UJust (max w w')
