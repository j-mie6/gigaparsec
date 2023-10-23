{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-all-missed-specialisations #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Errors (module Text.Gigaparsec.Internal.Errors) where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set (toList)

import Text.Gigaparsec.Errors.ErrorBuilder (formatDefault, formatPosDefault, vanillaErrorDefault, specialisedErrorDefault, combineMessagesDefault, disjunct, endOfInputDefault, namedDefault, rawDefault, unexpectedDefault)

type CaretWidth :: *
data CaretWidth = FlexibleCaret { width :: {-# UNPACK #-} !Word }
                | RigidCaret { width :: {-# UNPACK #-} !Word }
                deriving stock Eq

isFlexible :: CaretWidth -> Bool
isFlexible FlexibleCaret{} = True
isFlexible _               = False

-- First pass of the error system will just use a `show` instance within parse,
-- and the tests will use a different parse that exposes the underlying datatype.
-- this will be improved when the error builder is introduced.

type ParseError :: *
data ParseError = VanillaError { offset :: {-# UNPACK #-} !Word
                               , line :: {-# UNPACK #-} !Word
                               , col :: {-# UNPACK #-} !Word
                               , unexpected :: !(Either Int UnexpectItem)
                               , expecteds :: !(Set ExpectItem)
                               , reasons :: !(Set String)
                               , lexicalError :: !Bool
                               }
                | SpecialisedError { offset :: {-# UNPACK #-} !Word
                                   , line :: {-# UNPACK #-} !Word
                                   , col :: {-# UNPACK #-} !Word
                                   , msgs :: ![String]
                                   , caretWidth :: {-# UNPACK #-} !Word
                                   }
                deriving stock Eq

type Input :: *
type Input = NonEmpty Char
type UnexpectItem :: *
data UnexpectItem = UnexpectRaw !Input {-# UNPACK #-} !Word
                  | UnexpectNamed !String !CaretWidth
                  | UnexpectEndOfInput
                  deriving stock Eq
type ExpectItem :: *
data ExpectItem = ExpectRaw !String
                | ExpectNamed !String
                | ExpectEndOfInput
                deriving stock (Eq, Ord)

instance Show ParseError where
  show :: ParseError -> String
  show err = formatDefault (formatPosDefault (line err) (col err)) Nothing
                           (formatErr err)
    where formatErr VanillaError{..} =
            vanillaErrorDefault (unexpectedDefault Nothing)
                                (disjunct True (map formatExpectItem (Set.toList expecteds)))
                                (combineMessagesDefault reasons)
                                []
          formatErr SpecialisedError{..} =
            specialisedErrorDefault (combineMessagesDefault msgs)
                                    []

          formatExpectItem (ExpectRaw raw) = rawDefault raw
          formatExpectItem (ExpectNamed name) = namedDefault name
          formatExpectItem ExpectEndOfInput = endOfInputDefault

          --formatUnexpectItem (UnexpectRaw raw) = rawDefault raw
