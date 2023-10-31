{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts #-}
module Text.Gigaparsec.Errors.ErrorBuilder (module Text.Gigaparsec.Errors.ErrorBuilder) where

import Data.Kind (Constraint)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)

import Text.Gigaparsec.Errors.Token (Token)

type ErrorBuilder :: * -> Constraint
class (Ord (Item err)) => ErrorBuilder err where
  format :: Position err -> Source err -> ErrorInfoLines err -> err

  type Position err
  type Source err
  pos :: Word -> Word -> Position err
  source :: Maybe String -> Source err

  type ErrorInfoLines err
  vanillaError :: UnexpectedLine err -> ExpectedLine err -> Messages err -> LineInfo err -> ErrorInfoLines err
  specialisedError :: Messages err -> LineInfo err -> ErrorInfoLines err

  type ExpectedItems err
  type Messages err

  combineExpectedItems :: Set (Item err) -> ExpectedItems err
  combineMessages :: [Message err] -> Messages err

  type UnexpectedLine err
  type ExpectedLine err
  type Message err
  type LineInfo err

  unexpected :: Maybe (Item err) -> UnexpectedLine err
  expected :: ExpectedItems err -> ExpectedLine err
  reason :: String -> Message err
  message :: String -> Message err

  lineInfo :: String -> [String] -> [String] -> Word -> Word -> LineInfo err

  numLinesBefore :: Int
  numLinesAfter :: Int

  type Item err

  raw :: String -> Item err
  named :: String -> Item err
  endOfInput :: Item err

  unexpectedToken :: NonEmpty Char -> Word -> Bool -> Token

--instance ErrorBuilder String where
