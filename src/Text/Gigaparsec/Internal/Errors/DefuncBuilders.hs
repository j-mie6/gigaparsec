{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, DataKinds, UnboxedTuples #-}
module Text.Gigaparsec.Internal.Errors.DefuncBuilders (
    --module Text.Gigaparsec.Internal.Errors.DefuncBuilders
    asParseError
  ) where

import Text.Gigaparsec.Internal.Errors.DefuncTypes
import Text.Gigaparsec.Internal.Errors (ParseError(VanillaError, SpecialisedError), CaretWidth)
import Text.Gigaparsec.Internal.Errors.DefuncError (isLexical)
import Text.Gigaparsec.Internal.Errors.ErrorItem

import Data.Set (Set)
import Data.Set qualified as Set

asParseError :: DefuncError -> ParseError
asParseError e@DefuncError{..} = case errKind of
  IsVanilla -> case makeVanilla 0 0 Set.empty EmptyItem Set.empty True errTy of
    (# line, col, exs, unex, reasons #) ->
      VanillaError presentationOffset line col (toErrorItem unex) exs reasons (isLexical e) 0 0
  IsSpecialised -> undefined
  where
    makeVanilla :: Word -> Word -> Set ExpectItem -> BuilderUnexpectItem -> Set String -> Bool
                -> DefuncError_ 'Vanilla
                -> (# Word, Word, Set ExpectItem, BuilderUnexpectItem, Set String #)
    makeVanilla !_ !_ !exs !unex !reasons !acceptingExpected (Base line col err) =
      case err of
        Empty unexWidth ->
          (# line, col, exs, updateEmptyUnexpected unexWidth unex, reasons #)
        Expected exs' unexWidth ->
          (# line, col, addLabels acceptingExpected exs exs', updateUnexpected unexWidth unex, reasons #)
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
              {-
              builder.whenAcceptingExpected {
                for (size <- hints.updateExpectedsAndGetSize(builder)) builder.updateUnexpected(size)
              }
              -}
              undefined
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
type BuilderUnexpectItem :: *
data BuilderUnexpectItem =
  EmptyItem

updateEmptyUnexpected :: Word -> BuilderUnexpectItem -> BuilderUnexpectItem
updateEmptyUnexpected _ _ = undefined -- TODO:

updateUnexpected :: Word -> BuilderUnexpectItem -> BuilderUnexpectItem
updateUnexpected _ _ = undefined  -- TODO:

updateUnexpected' :: String -> CaretWidth -> BuilderUnexpectItem -> BuilderUnexpectItem
updateUnexpected' _ _ _ = undefined -- TODO:

addLabels :: Bool -> Set ExpectItem -> Set ExpectItem -> Set ExpectItem
addLabels True !exs !exs' = Set.union exs exs'
addLabels False exs _     = exs

toErrorItem :: BuilderUnexpectItem -> Either Word UnexpectItem
toErrorItem _ = undefined  -- TODO:
