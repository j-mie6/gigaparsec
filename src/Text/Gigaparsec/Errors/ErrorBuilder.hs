{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, FlexibleInstances, FlexibleContexts #-}
module Text.Gigaparsec.Errors.ErrorBuilder (module Text.Gigaparsec.Errors.ErrorBuilder) where

import Data.Kind (Constraint)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty((:|)))

import Text.Gigaparsec.Errors.Token (Token(Named, Raw))
import safe Text.Gigaparsec.Errors.DefaultErrorBuilder ( StringBuilder, formatDefault
                                                       , vanillaErrorDefault, specialisedErrorDefault
                                                       , rawDefault, namedDefault, endOfInputDefault
                                                       , expectedDefault, unexpectedDefault
                                                       , disjunct, combineMessagesDefault
                                                       , formatPosDefault, lineInfoDefault
                                                       )
import Data.String (IsString(fromString))

import Data.Set qualified as Set (toList)
import Data.Char (isSpace, generalCategory, ord, GeneralCategory(Format, Surrogate, PrivateUse, NotAssigned, Control))
import Numeric (showHex)

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

instance ErrorBuilder String where
  format = formatDefault

  type Position String = StringBuilder
  type Source String = Maybe StringBuilder

  pos = formatPosDefault
  source = fmap fromString

  type ErrorInfoLines String = [StringBuilder]
  vanillaError = vanillaErrorDefault
  specialisedError = specialisedErrorDefault

  type ExpectedItems String = Maybe StringBuilder
  type Messages String = [StringBuilder]

  combineExpectedItems = disjunct True . Set.toList
  combineMessages = combineMessagesDefault

  type UnexpectedLine String = Maybe StringBuilder
  type ExpectedLine String = Maybe StringBuilder
  type Message String = String
  type LineInfo String = [StringBuilder]

  unexpected = unexpectedDefault
  expected = expectedDefault
  reason = id
  message = id

  lineInfo = lineInfoDefault

  numLinesBefore = 1
  numLinesAfter = 1

  type Item String = String

  raw = rawDefault
  named = namedDefault
  endOfInput = endOfInputDefault

  -- TillNextWhitespace with matches parser demand
  unexpectedToken ('\n' :| _) _ _ = Named "newline" 1
  unexpectedToken ('\r' :| _) _ _ = Named "carriage return" 1
  unexpectedToken ('\t' :| _) _ _ = Named "tab" 1
  unexpectedToken (' ' :| _) _ _ = Named "space" 1
  unexpectedToken (c :| cs) parserDemanded _
    | isSpace c = Named "whitespace character" 1
    | otherwise = case generalCategory c of
                    Format -> unprintable
                    Surrogate -> unprintable
                    PrivateUse -> unprintable
                    NotAssigned -> unprintable
                    Control -> unprintable
                    _ -> Raw (take (fromIntegral parserDemanded) (tillNextWhitespace (c:cs)))
    where unprintable = Named ("non-printable character (\\x" ++ showHex (ord c) ")") 1
          tillNextWhitespace = takeWhile (not . isSpace)
