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

{-|
This type configures both errors that make labels and those that make reasons.
-}
type LabelWithExplainConfig :: *
data LabelWithExplainConfig 
  -- | No special labels or reasons should be generated, and default errors should be used instead.
  = LENotConfigured
  -- | The configuration produces the labels in the given set, which should be non-empty.
  | LELabel !(Set String)
  -- | The error should be displayed using the given reason.
  | LEReason !String
  -- | This label should be hidden.
  | LEHidden
  -- | The configuration produces the labels in the given set, and provides the given reason.
  | LELabelAndReason !(Set String) !String

{-|
This type configures errors that make labels.
-}
type LabelConfig :: *
data LabelConfig
  -- | No special labels should be generated, and default errors should be used instead.
  = LNotConfigured
  -- | The configuration produces the labels in the given set, which should not be empty.
  | LLabel !(Set String)
  -- | This label should be hidden.
  | LHidden

{-|
This type configures errors that give reasons.
-}
type ExplainConfig :: *
data ExplainConfig 
  -- | No special reasons should be generated, and default errors should be used instead.
  = ENotConfigured
  -- | The error should be displayed using the given reason.
  | EReason !String

{-|
A type @config@ is an 'Annotate' if it can be used to attach extra information to a 'Parsec' parser.

These annotations may consist of, for example: 

- Labels ('LabelConfig'), which give a parser a name (or names) they can be referred to by.
- Reasons for errors ('ExplainConfig'), which will supply a reason for when a parser produces an error.
-}
type Annotate :: * -> Constraint
class Annotate config where
  -- | Annotate the given parser according to the @config@.
  annotate :: config   -- ^ The configuration controlling the annotation.
           -> Parsec a -- ^ The parser to annotate
           -> Parsec a -- ^ An annotated parser.

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

{-|
Configures how filters should be used within the 'Text.Gigaparsec.Token.Lexer.Lexer'.
-}
type FilterConfig :: * -> *
data FilterConfig a 
  -- | No error configuration for the filter is specified; a regular filter is used instead.
  = VSBasicFilter
  {-| 
  Ensure the filter will generate specialised messages for the given failing parse.
   
  Usage: @'VSSpecializedFilter' message@, where

  - @message@: a function producing the message for the given value.
  -}
  | VSSpecializedFilter 
    (a -> NonEmpty String) -- ^ a function producing the message for the given value.
  {-|
  Ensure the filter generates a /vanilla/ unexpected item for the given failing parse.

  Usage: @'VSUnexpected' unexpected@, where

  - @unexpected@: a function producing the unexpected label for the given value.
  -}
  | VSUnexpected (a -> String)
  {-| 
  Ensure that the filter will generate a /vanilla/ reason for the given failing parse.
  
  Usage: @'VSBecause' reason@, where

  - @reason@: a function producing the reason for the given value.
  -}
  | VSBecause 
    (a -> String) -- ^ a function producing the reason for the given value.
  {-| 
  The filter generates a /vanilla/ unexpected item, and a reason for the given failing parse.
  
  Usage: @'VSUnexpectedBecause' reason unexpected@, where

  - @reason@: a function producing the reason for the given value.
  - @unexpected@: a function producing the unexpected label for the given value.
  -}
  | VSUnexpectedBecause 
    (a -> String) -- ^ a function producing the reason for the given value.
    (a -> String) -- ^ a function producing the unexpected label for the given value.

{-|
Specifies that only filters generating /vanilla/ errors can be used.
-}
type VanillaFilterConfig :: * -> *
data VanillaFilterConfig a 
  -- | No error configuration for the filter is specified; a regular filter is used instead.
  = VBasicFilter
  {-|
  Ensure the filter generates a /vanilla/ unexpected item for the given failing parse.

  Usage: @'VUnexpected' unexpected@, where

  - @unexpected@: a function producing the unexpected label for the given value.
  -}
  | VUnexpected 
    (a -> String) -- ^ a function producing the unexpected label for the given value.
  | VBecause 
    (a -> String)
  {-| 
  The filter generates a /vanilla/ unexpected item, and a reason for the given failing parse.
  
  Usage: @'VUnexpectedBecause' reason unexpected@, where

  - @reason@: a function producing the reason for the given value.
  - @unexpected@: a function producing the unexpected label for the given value.
  -}
  | VUnexpectedBecause 
    (a -> String) -- ^ a function producing the reason for the given value.
    (a -> String) -- ^ a function producing the unexpected label for the given value.

{-|
Specifies that only filters generating /specialised/ errors can be used.
-}
type SpecializedFilterConfig :: * -> *
data SpecializedFilterConfig a 
  -- | No error configuration for the filter is specified; a regular filter is used instead.
  = SBasicFilter
  {-| 
  Ensure the filter will generate specialised messages for the given failing parse.
   
  Usage: @'SSpecializedFilter' message@, where

  - @message@: a function producing the message for the given value.
  -}
  | SSpecializedFilter 
    (a -> NonEmpty String) -- ^ a function producing the message for the given value.


{-|
A type @config@ is a 'Filter' when it describes how to process the results of a 'Errors.filterS' or 'Errors.mapMaybeS' on a parser.

The @config@ may allow for these results to have more specialised error messages.
-}
type Filter :: (* -> *) -> Constraint
class Filter config where
  {-|
  Filter a parser according to a predicate, use the @config@ to improve the error message if the predicate fails.

  This combinator filters the result of this parser using a given predicate, succeeding only if the predicate returns true;
  if the predicate fails, the @config@ is used to elaborate the error message.

  This will likely have the same success/failure behaviour as 'Errors.filterS', except the messages output by failure will
  be changed according to the @config@.
  -}
  filterS :: config a     -- ^ The configuration which alters the failure message.
          -> (a -> Bool)  -- ^ @pred@, the predicate to filter by.
          -> Parsec a     -- ^ @p@, the parser whose results are to be filtered.
          -> Parsec a     -- ^ a parser that returns the result of @p@ if it passes @pred@;
                          --   if @pred@ fails, then the error message is altered according to the config.
  filterS = filterS' id
  {-|
  This combinator filters the result of this parser using a given predicate, succeeding only if the predicate returns @Just x@ for some @x@;
  if the predicate fails, the @config@ is used to elaborate the error message.

  This will likely have the same success/failure behaviour as 'Errors.mapMaybeS', except the messages output by failure will
  be changed according to the @config@.
  -}
  mapMaybeS :: config a       -- ^ The configuration which alters the failure message.
            -> (a -> Maybe b) -- ^ @pred@, the predicate to filter by.
            -> Parsec a       -- ^ @p@, the parser whose results are to be filtered.
            -> Parsec b       -- ^ a parser that returns the result of @pred@ applied to that of @p@;
                              --   if @pred@ returns @Nothing@, then the error message is altered according to the config.
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


{-|
Configures what error should be generated when illegal characters in a string or character literal are parsable.
-}
type VerifiedBadChars :: *
data VerifiedBadChars 
  {-|
  "bad literal chars" generate a bunch of given messages in a specialised error. 
  The map sends bad characters to their messages.
  -}
  = BadCharsFail !(Map Char (NonEmpty String))
  {-|
  "bad literal chars" generate a reason as a /vanilla/ error. 
  The map sends bad characters to their reasons.
  -}
  | BadCharsReason !(Map Char String)
  {-|
  Disable the verified error for bad characters: 
  this may improve parsing performance slightly on the failure case.
  -}
  | BadCharsUnverified

checkBadChar :: VerifiedBadChars -> Parsec a
checkBadChar (BadCharsFail cs) = verifiedFail (NonEmpty.toList . (cs Map.!)) (satisfy (`Map.member` cs))
checkBadChar (BadCharsReason cs) = verifiedExplain (cs Map.!) (satisfy (`Map.member` cs))
checkBadChar BadCharsUnverified = empty
