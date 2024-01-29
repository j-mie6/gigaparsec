{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs, DataKinds, UnboxedTuples, PatternSynonyms, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- Yes, this is redundant, however, it is necessary to get the UNPACK to fire
{-# OPTIONS_GHC -Wno-redundant-strictness-flags -Wno-missing-kind-signatures #-}
#include "portable-unlifted.h"
module Text.Gigaparsec.Internal.Errors.DefuncBuilders (
    asParseError
  ) where

import Text.Gigaparsec.Internal.Errors.DefuncTypes (
    DefuncHints(Blank, AddErr, Replace),
    ErrorOp(Amended, WithLabel, WithHints, Merged, WithReason, AdjustCaret),
    BaseError(Unexpected, Empty, Expected, ClassicSpecialised),
    DefuncError_(Op, Base),
    DefuncError(DefuncError, presentationOffset, errKind, errTy),
    ErrKindSingleton(IsSpecialised, IsVanilla),
    ErrKind(Vanilla, Specialised),
    expecteds, unexpectedWidth
  )
import Text.Gigaparsec.Internal.Errors.ParseError (ParseError(VanillaError, SpecialisedError))
import Text.Gigaparsec.Internal.Errors.CaretControl (CaretWidth(FlexibleCaret, width), isFlexible)
import Text.Gigaparsec.Internal.Errors.DefuncError (isLexical)
import Text.Gigaparsec.Internal.Errors.ErrorItem (
    ExpectItem(ExpectNamed),
    UnexpectItem(UnexpectEndOfInput, UnexpectNamed, UnexpectRaw)
  )

import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, union, member, map)
import Data.List.NonEmpty (nonEmpty)

CPP_import_PortableUnlifted

asParseError :: String -> DefuncError -> ParseError
asParseError !input e@DefuncError{..} = case errKind of
  IsVanilla -> case makeVanilla 0 0 Set.empty (NoItem 0) Set.empty True errTy of
    (# line, col, exs, unex, reasons #) ->
      VanillaError presentationOffset line col (toErrorItem input presentationOffset unex) exs reasons (isLexical e)
  IsSpecialised -> case makeSpec 0 0 0 True id errTy of
    (# line, col, width, _, dmsgs #) ->
      SpecialisedError presentationOffset line col (distinct (dmsgs [])) width
  where
    !outOfRange = presentationOffset >= fromIntegral (length input)

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

    makeSpec :: Word -> Word -> Word -> Bool -> ([String] -> [String])
             -> DefuncError_ 'Specialised
             -> (# Word, Word, Word, Bool, [String] -> [String] #)
    makeSpec !_ !_ !w !flexible !dmsgs (Base line col (ClassicSpecialised msgs cw)) =
      let (# w', flexible' #) = updateCaretWidth flexible cw w
      in (# line, col, w', flexible', dmsgs . (msgs ++) #)
    makeSpec line col w flexible dmsgs (Op op) = case op of
      Merged err1 err2->
        case makeSpec line col w flexible dmsgs err1 of
          (# line', col', w', flexible', dmsgs' #) ->
            makeSpec line' col' w' flexible' dmsgs' err2
      AdjustCaret err1 err2 ->
        case makeSpec line col w flexible dmsgs err1 of
          (# line', col', w', flexible', dmsgs' #) ->
              -- assuming flexible == True
              (# line', col', adjustCaret w' err2, flexible', dmsgs' #)
      Amended line' col' err -> case makeSpec line col w flexible dmsgs err of
        (# _, _, w', flexible', dmsgs' #) -> (# line', col', w', flexible', dmsgs' #)

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
pickHigher EndOfInput _ = EndOfInput
pickHigher _ EndOfInput = EndOfInput
pickHigher x@(RawItem w1) y@(RawItem w2)
  | w1 > w2   = x
  | otherwise = y
pickHigher x@(NoItem w1) y@(NoItem w2)
  | w1 > w2   = x
  | otherwise = y
pickHigher x@(NamedItem _ cw1) y@(NamedItem _ cw2)
  | isFlexible cw1 /= isFlexible cw2 = if isFlexible cw1 then x else y
  | width cw1 > width cw2            = x
  | otherwise                        = y
pickHigher x@(RawItem w1) (NoItem w2)
  | w1 > w2   = x
  | otherwise = RawItem w2
pickHigher x@(NamedItem name (FlexibleCaret w1)) (RawItem w2)
  | w1 > w2   = x
  | otherwise = NamedItem name (FlexibleCaret w2)
pickHigher x@(NamedItem name (FlexibleCaret w1)) (NoItem w2)
  | w1 > w2   = x
  | otherwise = NamedItem name (FlexibleCaret w2)
pickHigher x@NamedItem{} _ = x
pickHigher x y = pickHigher y x

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

type UMaybe a = (# (# #) | a #)
{-# COMPLETE UJust, UNothing #-}
pattern UJust :: a -> UMaybe a
pattern UJust x = (# | x #)
pattern UNothing :: UMaybe a
pattern UNothing = (# (# #) | #)

collectHints :: Set ExpectItem -> UMaybe Word -> DefuncHints -> (# Set ExpectItem, UMaybe Word #)
collectHints !exs width Blank = (# exs, width #)
collectHints exs width (Replace ls) = (# Set.union exs (Set.map ExpectNamed ls), width #)
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

distinct :: forall a. Ord a => [a] -> [a]
distinct = go Set.empty
  where
    go :: Set a -> [a] -> [a]
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs

updateCaretWidth :: Bool -> CaretWidth -> Word -> (# Word, Bool #)
updateCaretWidth flexible cw !w
  | isFlexible cw == flexible = (# max (width cw) w, flexible #)
  | isFlexible cw             = (# w, flexible #)
  | otherwise                 = (# width cw, False #)

adjustCaret :: Word -> DefuncError_ 'Vanilla -> Word
adjustCaret w (Base _ _ err) = max (unexpectedWidth err) w
adjustCaret w (Op op) = case op of
  WithLabel err _  -> adjustCaret w err
  WithHints err _  -> adjustCaret w err
  WithReason err _ -> adjustCaret w err
  Amended _ _ err  -> adjustCaret w err
  Merged err1 err2 -> adjustCaret (adjustCaret w err1) err2
