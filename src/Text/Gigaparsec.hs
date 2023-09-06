{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Text.Gigaparsec
Description : Contains the bulk of the core combinators.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This object contains the core combinators and parser type: all parsers will require something from
within!

TODO: what is inside it?

@since 0.1.0.0
-}
module Text.Gigaparsec (
    Parsec, Result(..), parse,
  -- * Primitive Combinators
  -- | These combinators are specific to parser combinators. In one way or another, they influence
  -- how a parser consumes input, or under what conditions a parser does or does not fail. These are
  -- really important for most practical parsing considerations, although lookAhead is much less
  -- well used.
    atomic, lookAhead, notFollowedBy, eof,
  -- * Consumptionless Parsers
  -- | These combinators and parsers do not consume input: they are the most primitive ways of
  -- producing successes and failures with the minimal possible effect on the parse. They are,
  -- however, reasonably useful; in particular, `pure` and `unit` can be put to good use in
  -- injecting results into a parser without needing to consume anything, or mapping another parser.
    unit,
  -- *** Re-exported from "Control.Applicative"
    pure, empty,
  -- * Result Changing Combinators
  -- | These combinators change the result of the parser they are called on into a value of a
  -- different type. This new result value may or may not be derived from the previous result.
    ($>),
  -- *** Re-exported from "Data.Functor"
    (<$>), (<$), void,
  -- * Sequencing Combinators
  -- | These combinators all combine two parsers in sequence. The first argument of the combinator
  -- will be executed first, then the second argument second. The results of both parsers are
  -- combined in some way (depending on the individual combinator). If one of the parsers fails, the
  -- combinator as a whole fails.
    (<~>), (<:>),
  -- *** Re-exported from "Control.Applicative"
    (<*>), liftA2, (*>), (<*), (<**>),
  -- * Branching Combinators
  -- | These combinators allow for parsing one alternative or another. All of these combinators are
  -- /left-biased/, which means that the left-hand side of the combinator is tried first: the
  -- right-hand side of the combinator will only be tried when the left-hand one failed (and did not
  -- consume input in the process).
    (<+>),
  -- *** Re-exported from "Control.Applicative"
    (<|>),
  -- * Selective Combinators
  -- | These combinators will decide which branch to take next based on the result of another parser.
  -- This differs from combinators like `(<|>)` which make decisions based on the success/failure of
  -- a parser: here the result of a /successful/ parse will direct which option is done.

  -- *** Re-exported from "Control.Selective"
    select, branch,

  -- * Filtering Combinators
  -- | These combinators perform filtering on the results of a parser. This means that, given the
  -- result of a parser, they will perform some function on that result, and the success of that
  -- function effects whether or not the parser fails.

  -- * Folding Combinators
  -- | These combinators repeatedly execute a parser (at least zero or one times depending on the
  -- specific combinator) until it fails. The results of the successes are then combined together
  -- using a folding function. An initial value for the accumulation may be given (for the folds),
  -- or the first successful result is the initial accumulator (for the reduces). These are
  -- implemented efficiently and do not need to construct any intermediate list with which to store
  -- the results.
    many, some, -- TODO: these need to be properly categorised
  ) where

-- NOTE:
-- This module is mostly just for re-exports, though there may be primitive
-- combinators in here too. If user can see it, it can go in here.
--
-- Care MUST be taken to not expose /any/ implementation details from
-- `Internal`: when they are in the public API, we are locked into them!

import Text.Gigaparsec.Internal (Parsec(Parsec), emptyState)
import Text.Gigaparsec.Internal qualified as Internal.State (State(..))
import Text.Gigaparsec.Internal.RT (runRT)

import Data.Functor (void)
import Control.Applicative (liftA2, (<|>), empty, many, some, (<**>)) -- liftA2 required until 9.6
import Control.Selective (select, branch)

-- Hiding the Internal module seems like the better bet: nobody needs to see it anyway :)
-- re-expose like this to prevent hlint suggesting import refinement into internal
--type Parsec :: * -> *
--type Parsec = Internal.Parsec

type Result :: * -> *
data Result a = Success a | Failure deriving stock (Show, Eq)

parse :: Parsec a -> String -> Result a
parse (Parsec p) inp = runRT $ p (emptyState inp) good bad
  where good x _ = return (Success x)
        bad _    = return Failure

{-|
This combinator parses its argument @p@, but rolls back any consumed input on failure.

If the parser @p@ succeeds, then @atomic p@ has no effect. However, if @p@ failed,
then any input that it consumed is rolled back. This has two uses: it ensures that
the parser @p@ is all-or-nothing when consuming input, and it allows for
parsers that consume input to backtrack when they fail (with '(<|>)'). It should be
used for the latter purpose sparingly, however, since excessive backtracking in a
parser can result in much lower efficiency.

>>> parse (string "abc" <|> string "abd") "abd"
Failure .. -- first parser consumed a, so no backtrack
>>> parse (atomic (string "abc") <|> string "abd") "abd"
Success "abd" -- first parser does not consume input on failure now

@since 0.1.0.0
-}
atomic :: Parsec a -- ^ the parser, @p@, to execute, if it fails, it will not have consumed input.
       -> Parsec a -- ^ a parser that tries @p@, but never consumes input if it fails.
atomic (Parsec p) = Parsec $ \st ok err ->
  p st ok (const $ err st)

{-| This combinator parses its argument @p@, but does not consume input if it succeeds.

If the parser @p@ succeeds, then @lookAhead p@ will roll back any input consumed
whilst parsing @p@. If @p@ fails, however, then the whole combinator fails and
any input consumed __remains consumed__. If this behaviour is not desirable,
consider pairing `lookAhead` with `atomic`.

==== __Examples__
>>> parse (lookAhead (string "aaa") *> string "aaa") "aaa"
Success "aaa"
>>> parse (lookAhead (string "abc") <|> string "abd" "abd"
Failure .. -- lookAhead does not roll back input consumed on failure

@since 0.1.0.0
-}
lookAhead :: Parsec a -- ^ the parser, @p@, to execute
          -> Parsec a -- ^ a parser that parses @p@ and never consumes input if it succeeds.
lookAhead (Parsec p) = Parsec $ \st ok err ->
  p st (\x _ -> ok x st) err

{-|
This combinator parses its argument @p@, and succeeds when @p@ fails and vice-versa, never consuming
input.

If the parser @p@ succeeds, then @notFollowedBy p@ will fail, consuming no input.
Otherwise, should @p@ fail, then @notFollowedBy p@ will succeed, consuming no input
and returning @()@.

==== __Examples__
One use for this combinator is to allow for \"longest-match\" behaviour.
For instance, keywords are normally only considered keywords if they are not
part of some larger valid identifier (i.e. the keyword \"if\" should not parse
successfully given \"ifp\"). This can be accomplished as follows:

@
keyword :: String -> Parsec ()
keyword kw = atomic $ string kw *> notFollowedBy letterOrDigit
@

@since 0.1.0.0
-}
notFollowedBy :: Parsec a  -- ^ the parser, @p@, to execute, it must fail in order for this combinator to succeed.
              -> Parsec () -- ^ a parser which fails when @p@ succeeds and succeeds otherwise, never consuming input.
notFollowedBy (Parsec p) = Parsec $ \st ok err ->
  p st (\_ _ -> err st) (\_ -> ok () st)

-- eof is usually `notFollowedBy item`, but this requires annoying cyclic dependencies on Char
{- This parser only succeeds at the end of the input.

Equivalent to `notFollowedBy(item)`.

==== __Examples__
>>> parse eof "a"
Failure ..
>>> parse eof ""
Success ()

@since 0.1.0.0
-}
eof :: Parsec ()
eof = Parsec $ \st good bad -> case Internal.State.input st of
  (:){} -> bad st
  []    -> good () st

{-|
This parser produces @()@ without having any other effect.

When this parser is ran, no input is required, nor consumed, and the given value will always be
successfully returned. It has no other effect on the state of the parser.

@since 0.1.0.0
-}
unit :: Parsec ()
unit = pure ()

infixl 4 <~>
(<~>) :: Parsec a -> Parsec b -> Parsec (a, b)
(<~>) = liftA2 (,)

infixl 4 $>
($>) :: Parsec a -> b -> Parsec b
($>) = flip (<$)

infixl 4 <:>
(<:>) :: Parsec a -> Parsec [a] -> Parsec [a]
(<:>) = liftA2 (:)

infixl 3 <+>
(<+>) :: Parsec a -> Parsec b -> Parsec (Either a b)
p <+> q = Left <$> p <|> Right <$> q
