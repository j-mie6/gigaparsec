{-# LANGUAGE Safe #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Errors (module Text.Gigaparsec.Internal.Token.Errors) where

import Text.Gigaparsec (Parsec, empty)
import Text.Gigaparsec qualified as Errors (filterS, mapMaybeS)
import Text.Gigaparsec.Char (satisfy)
import Text.Gigaparsec.Errors.Combinator qualified as Errors (
    label, explain, hide,
    filterOut, guardAgainst, mapEitherS, unexpectedWhen, unexpectedWithReasonWhen
  )
import Text.Gigaparsec.Errors.Patterns (verifiedFail, verifiedExplain)

import Data.Set (Set)
import Data.Map (Map)

import Data.Map qualified as Map (member, (!))
import Data.Kind (Constraint)
import Data.Maybe (isJust, fromJust)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)

type LabelWithExplainConfig :: *
data LabelWithExplainConfig = LENotConfigured
                            | LELabel !(Set String)
                            | LEReason !String
                            | LEHidden
                            | LELabelAndReason !(Set String) !String

type LabelConfig :: *
data LabelConfig = LNotConfigured
                 | LLabel !(Set String)
                 | LHidden

type ExplainConfig :: *
data ExplainConfig = ENotConfigured
                   | EReason !String

type Annotate :: * -> Constraint
class Annotate config where
  annotate :: config -> Parsec a -> Parsec a

instance Annotate LabelConfig where
  annotate LNotConfigured = id
  annotate (LLabel ls) = Errors.label ls
  annotate LHidden = Errors.hide

instance Annotate ExplainConfig where
  annotate ENotConfigured = id
  annotate (EReason r) = Errors.explain r

instance Annotate LabelWithExplainConfig where
  annotate LENotConfigured = id
  annotate (LELabel ls) = Errors.label ls
  annotate LEHidden = Errors.hide
  annotate (LEReason r) = Errors.explain r
  annotate (LELabelAndReason ls r) = Errors.label ls . Errors.explain r

type FilterConfig :: * -> *
data FilterConfig a = VSBasicFilter
                    | VSSpecializedFilter (a -> NonEmpty String)
                    | VSUnexpected (a -> String)
                    | VSBecause (a -> String)
                    | VSUnexpectedBecause (a -> String) (a -> String)

type VanillaFilterConfig :: * -> *
data VanillaFilterConfig a = VBasicFilter
                           | VUnexpected (a -> String)
                           | VBecause (a -> String)
                           | VUnexpectedBecause (a -> String) (a -> String)

type SpecializedFilterConfig :: * -> *
data SpecializedFilterConfig a = SBasicFilter
                               | SSpecializedFilter (a -> NonEmpty String)

type Filter :: (* -> *) -> Constraint
class Filter config where
  filterS :: config a -> (a -> Bool) -> Parsec a -> Parsec a
  filterS = filterS' id
  mapMaybeS :: config a -> (a -> Maybe b) -> Parsec a -> Parsec b
  mapMaybeS = mapMaybeS' id

  filterS' :: (a -> x) -> config x -> (a -> Bool) -> Parsec a -> Parsec a
  mapMaybeS' :: (a -> x) -> config x -> (a -> Maybe b) -> Parsec a -> Parsec b

instance Filter FilterConfig where
  filterS' _ VSBasicFilter g = Errors.filterS g
  filterS' f (VSSpecializedFilter msgs) g = Errors.guardAgainst (errWhen f (NonEmpty.toList . msgs) g)
  filterS' f (VSBecause reason) g = Errors.filterOut (errWhen f reason g)
  filterS' f (VSUnexpected unex) g = Errors.unexpectedWhen (errWhen f unex g)
  filterS' f (VSUnexpectedBecause unex reason) g =
    Errors.unexpectedWithReasonWhen (errWhen f (\x -> (unex x, reason x)) g)

  mapMaybeS' _ VSBasicFilter g = Errors.mapMaybeS g
  mapMaybeS' f (VSSpecializedFilter msgs) g = Errors.mapEitherS (errMap f msgs g)
  mapMaybeS' f config g = mapMaybeSDefault filterS' f config g

instance Filter VanillaFilterConfig where
  filterS' _ VBasicFilter g = Errors.filterS g
  filterS' f (VBecause reason) g = Errors.filterOut (errWhen f reason g)
  filterS' f (VUnexpected unex) g = Errors.unexpectedWhen (errWhen f unex g)
  filterS' f (VUnexpectedBecause unex reason) g =
    Errors.unexpectedWithReasonWhen (errWhen f (\x -> (unex x, reason x)) g)

  mapMaybeS' _ VBasicFilter g = Errors.mapMaybeS g
  mapMaybeS' f config g = mapMaybeSDefault filterS' f config g

instance Filter SpecializedFilterConfig where
  filterS' _ SBasicFilter g = Errors.filterS g
  filterS' f (SSpecializedFilter msgs) g = Errors.guardAgainst (errWhen f (NonEmpty.toList . msgs) g)

  mapMaybeS' _ SBasicFilter g = Errors.mapMaybeS g
  mapMaybeS' f (SSpecializedFilter msgs) g = Errors.mapEitherS (errMap f msgs g)

errWhen :: (a -> x) -> (x -> e) -> (a -> Bool) -> (a -> Maybe e)
errWhen f g p x
  | p x = Just (g (f x))
  | otherwise = Nothing

errMap :: (a -> x) -> (x -> e) -> (a -> Maybe b) -> (a -> Either e b)
errMap f g p x = maybe (Left (g (f x))) Right (p x)

mapMaybeSDefault :: ((a -> x) -> config x -> (a -> Bool) -> Parsec a -> Parsec a)
                 -> ((a -> x) -> config x -> (a -> Maybe b) -> Parsec a -> Parsec b)
mapMaybeSDefault filt f config g = fmap (fromJust . g) . filt f config (isJust . g)


type VerifiedBadChars :: *
data VerifiedBadChars = BadCharsFail !(Map Char (NonEmpty String))
                      | BadCharsReason !(Map Char String)
                      | BadCharsUnverified

checkBadChar :: VerifiedBadChars -> Parsec a
checkBadChar (BadCharsFail cs) = verifiedFail (NonEmpty.toList . (cs Map.!)) (satisfy (`Map.member` cs))
checkBadChar (BadCharsReason cs) = verifiedExplain (cs Map.!) (satisfy (`Map.member` cs))
checkBadChar BadCharsUnverified = empty
