{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
{-|
Module      : Text.Gigaparsec
Description : Contains the bulk of the core combinators.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This object contains the core combinators and parser type: all parsers will require something from
within!

@since 0.1.0.0
-}
module Text.Gigaparsec (
    Parsec, Result(..), result, parse, parseFromFile, parseRepl,
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
    filterS, mapMaybeS,

  -- * Folding Combinators
  -- | These combinators repeatedly execute a parser (at least zero or one times depending on the
  -- specific combinator) until it fails. The results of the successes are then combined together
  -- using a folding function. An initial value for the accumulation may be given (for the folds),
  -- or the first successful result is the initial accumulator (for the reduces). These are
  -- implemented efficiently and do not need to construct any intermediate list with which to store
  -- the results.
    many, some, manyl, manyr, somel, somer, manyMap, someMap,-- TODO: these need to be properly categorised
  ) where

-- NOTE:
-- This module is mostly just for re-exports, though there may be primitive
-- combinators in here too. If user can see it, it can go in here.
--
-- Care MUST be taken to not expose /any/ implementation details from
-- `Internal`: when they are in the public API, we are locked into them!

import Text.Gigaparsec.Internal (Parsec(Parsec), emptyState, manyr, somer)
import Text.Gigaparsec.Internal qualified as Internal (State(..), useHints, expectedErr)
import Text.Gigaparsec.Internal.RT qualified as Internal (RT, runRT, rtToIO)
import Text.Gigaparsec.Internal.Errors qualified as Internal (Error, ExpectItem(ExpectEndOfInput), fromError)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder)
import Text.Gigaparsec.Errors.Combinator (filterSWith, mapMaybeSWith)
import Text.Gigaparsec.Errors.ErrorGen (vanillaGen)

import Data.Functor (void)
import Control.Applicative (liftA2, (<|>), empty, many, some, (<**>)) -- liftA2 required until 9.6
import Control.Selective (select, branch)

import Data.Set qualified as Set (singleton, empty)
import GHC.Generics (Generic)

-- Hiding the Internal module seems like the better bet: nobody needs to see it anyway :)
-- re-expose like this to prevent hlint suggesting import refinement into internal
--type Parsec :: * -> *
--type Parsec = Internal.Parsec

{-|
Similar to @Either@, this type represents whether a parser has failed or not.

This is chosen instead of @Either@ to be more specific about the naming.
-}
type Result :: * -> * -> *
data Result e a = Success a | Failure e deriving stock (Show, Eq, Generic) -- TODO: monad?

{-|
A fold for the 'Result' type.

This functions like the 'either' function does for 'Either'.

@since 0.2.0.0
-}
result :: (e -> b) -> (a -> b) -> Result e a -> b
result _ success (Success x) = success x
result failure _ (Failure err) = failure err

{-# SPECIALISE parse :: Parsec a -> String -> Result String a #-}
{-# INLINABLE parse #-}
{-|
Runs a parser against some input.

Given a parser, some input, and a way of producing parse errors of a desired
type (via 'ErrorBuilder'), this function runs a parser to produce either a
successful result or an error. Note that the @err@ type parameter is first,
which allows for @parse \@String@ to make use of the defaultly formated @String@
error messages. This may not be required if it is clear from context. To make
this process nicer within GHCi, consider using 'parseRepl'.
-}
parse :: forall err a. ErrorBuilder err
      => Parsec a     -- ^ the parser to execute
      -> String       -- ^ the input to parse
      -> Result err a -- ^ result of the parse, either an error or result
parse p inp = Internal.runRT $ _parse Nothing p inp

{-|
Runs a parser against some input, pretty-printing the result to the terminal.

Compared, with 'parse', this function will always generate error messages as
strings and will print them nicely to the terminal (on multiple lines). If the
parser succeeds, the result will also be printed to the terminal. This is useful
for playing around with parsers in GHCi, seeing the results more clearly.

@since 0.2.0.0
-}
parseRepl :: Show a
          => Parsec a -- ^ the parser to execute
          -> String   -- ^ the input to parse
          -> IO ()
parseRepl p inp =
  do res <- Internal.rtToIO $ _parse Nothing p inp
     result putStrLn print res

{-# SPECIALISE parseFromFile :: Parsec a -> String -> IO (Result String a) #-}
{-# INLINABLE parseFromFile #-}
{-|
Runs a parser against some input obtained from a given file.

Given a parser, a filename, and a way of producing parse errors of a desired
type (via 'ErrorBuilder'), this function runs a parser to produce either a
successful result or an error. First, input is collected by reading the file,
and then the result is returned within `IO`; the filename is forwarded on to
the 'ErrorBuilder', which may mean it forms part of the generated error messages.

Note that the @err@ type parameter is first,
which allows for @parseFromFile \@String@ to make use of the defaultly formated @String@
error messages. This may not be required if it is clear from context.

@since 0.2.1.0
-}
parseFromFile :: forall err a. ErrorBuilder err
              => Parsec a -- ^ the parser to execute
              -> FilePath -- ^ the file to source the input from
              -> IO (Result err a) -- ^ the result of the parse, error or otherwise
parseFromFile p f =
  do inp <- readFile f
     Internal.rtToIO $ _parse (Just f) p inp

--TODO: parseFromHandle?

{-# INLINE _parse #-}
_parse :: forall err a. ErrorBuilder err => Maybe FilePath -> Parsec a -> String -> Internal.RT (Result err a)
_parse file (Parsec p) inp = p (emptyState inp) good bad
  where good :: a -> Internal.State -> Internal.RT (Result err a)
        good x _  = return (Success x)
        bad :: Internal.Error -> Internal.State -> Internal.RT (Result err a)
        bad err _ = return (Failure (Internal.fromError file inp err))

{-|
This combinator parses its argument @p@, but rolls back any consumed input on failure.

If the parser @p@ succeeds, then @atomic p@ has no effect. However, if @p@ failed,
then any input that it consumed is rolled back. This has two uses: it ensures that
the parser @p@ is all-or-nothing when consuming input, and it allows for
parsers that consume input to backtrack when they fail (with '(<|>)'). It should be
used for the latter purpose sparingly, however, since excessive backtracking in a
parser can result in much lower efficiency.

==== __Examples__
>>> parse (string "abc" <|> string "abd") "abd"
Failure .. -- first parser consumed a, so no backtrack
>>> parse (atomic (string "abc") <|> string "abd") "abd"
Success "abd" -- first parser does not consume input on failure now

@since 0.1.0.0
-}
atomic :: Parsec a -- ^ the parser, @p@, to execute, if it fails, it will not have consumed input.
       -> Parsec a -- ^ a parser that tries @p@, but never consumes input if it fails.
atomic (Parsec p) = Parsec $ \st ok bad -> p st ok (\err _ -> bad err st)

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
lookAhead (Parsec p) = Parsec $ \st ok err -> p st (\x _ -> ok x st) err

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

> keyword :: String -> Parsec ()
> keyword kw = atomic $ string kw *> notFollowedBy letterOrDigit

@since 0.1.0.0
-}
notFollowedBy :: Parsec a  -- ^ the parser, @p@, to execute, it must fail in order for this combinator to succeed.
              -> Parsec () -- ^ a parser which fails when @p@ succeeds and succeeds otherwise, never consuming input.
notFollowedBy (Parsec p) = Parsec $ \st ok bad ->
  p st (\_ st' -> let !width = Internal.consumed st' - Internal.consumed st
                  in Internal.useHints bad (Internal.expectedErr st Set.empty width) st)
       (\_ _ -> ok () st)

-- eof is usually `notFollowedBy item`, but this requires annoying cyclic dependencies on Char
{-| This parser only succeeds at the end of the input.

Equivalent to `notFollowedBy(item)`.

==== __Examples__
>>> parse eof "a"
Failure ..
>>> parse eof ""
Success ()

@since 0.1.0.0
-}
eof :: Parsec ()
eof = Parsec $ \st good bad -> case Internal.input st of
  (:){} -> Internal.useHints bad
             (Internal.expectedErr st (Set.singleton Internal.ExpectEndOfInput) 1) st
  []    -> good () st

{-|
This parser produces @()@ without having any other effect.

When this parser is ran, no input is required, nor consumed, and the given value will always be
successfully returned. It has no other effect on the state of the parser.

@since 0.1.0.0
-}
unit :: Parsec ()
unit = pure ()

{-|
This combinator, pronounced "zip", first parses this parser then parses @q@: if both succeed the result of this
parser is paired with the result of @q@.

First, this parser is ran, yielding @x@ on success, then @q@ is ran, yielding @y@ on success. If both
are successful then @(x, y)@ is returned. If either fail then the entire combinator fails.

==== __Examples__
>>> p = char 'a' <~> char 'b'
>>> parse p "ab"
Success ('a', 'b')
>>> parse p "b"
Failure ..
>>> parse p "a"
Failure ..
-}
infixl 4 <~>
(<~>) :: Parsec a -> Parsec b -> Parsec (a, b)
(<~>) = liftA2 (,)

{-|
This combinator, pronounced "as", replaces the result of this parser, ignoring the old result.

Similar to @(<$>)@, except the old result of this parser is not required to
compute the new result. This is useful when the result is a constant value (or function!).
Functionally the same as @p *> pure x@ or @const x <$> p@.

/In @parsley@, this combinator is known as @#>@ or @as@/.

==== __Examples__
>>> parse (string "true" $> true) "true"
Success true
-}
infixl 4 $>
($>) :: Parsec a -> b -> Parsec b
($>) = flip (<$)

{-|
This combinator, pronounced "cons", first parses this parser then parses @ps@: if both succeed the result of this
parser is prepended onto the result of @ps@.

First, this parser is ran, yielding @x@ on success, then @ps@ is ran, yielding @xs@ on success. If both
are successful then @x : xs@ is returned. If either fail then the entire combinator fails.

==== __Examples__
> some p = p <:> many(p)
-}
infixl 4 <:>
(<:>) :: Parsec a -> Parsec [a] -> Parsec [a]
(<:>) = liftA2 (:)

infixl 3 <+>
{-|
This combinator, pronounced "sum", wraps this parser's result in @Left@ if it succeeds, and parses @q@ if it failed __without__ consuming input,
wrapping the result in @Right@.

If this parser is successful, then its result is wrapped up using @Left@ and no further action is taken.
Otherwise, if this parser fails __without__ consuming input, then @q@ is parsed instead and its result is
wrapped up using @Right@. If this parser fails having consumed input, this combinator fails.
This is functionally equivalent to @Left <$> p <|> Right <$> q@.

The reason for this behaviour is that it prevents space leaks and improves error messages. If this behaviour
is not desired, use @atomic p@ to rollback any input consumed on failure.

==== __Examples__
>>> p = string "abc" <+> char "xyz"
>>> parse p "abc"
Success (Left "abc")
>>> parse p "xyz"
Success (Right "xyz")
>>> parse p "ab"
Failure .. -- first parser consumed an 'a'!
-}
(<+>) :: Parsec a -> Parsec b -> Parsec (Either a b)
p <+> q = Left <$> p <|> Right <$> q

{-|
This combinator will parse this parser __zero__ or more times combining the results with the function @f@ and base value @k@ from the left.

This parser will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a left-to-right
fashion using the function @f@: the left-most value provided to @f@ is the value @k@.
If this parser does fail at any point having consumed input, this combinator will fail.
-}
manyl :: (b -> a -> b) -- ^ function to apply to each value produced by this parser, starting at the left.
      -> b             -- ^  the initial value to feed into the reduction
      -> Parsec a
      -> Parsec b      -- ^ a parser which parses this parser many times and folds the results together with @f@ and @k@ left-associatively.
manyl f k = _repl f (pure k)

{-|
This combinator will parse this parser __one__ or more times combining the results with the function @f@ and base value @k@ from the left.

This parser will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a left-to-right
fashion using the function @f@: the left-most value provided to @f@ is the value @k@.
If this parser does fail at any point having consumed input, this combinator will fail.

==== __Examples__
> natural = somel (\x d -> x * 10 + digitToInt d) 0 digit
-}
somel :: (b -> a -> b) -- ^ function to apply to each value produced by this parser, starting at the left.
      -> b             -- ^  the initial value to feed into the reduction
      -> Parsec a
      -> Parsec b      -- ^ a parser which parses this parser some times and folds the results together with @f@ and @k@ left-associatively.
somel f k p = _repl f (f k <$> p) p

{-|
This combinator acts like the 'foldMap' function but with a parser.

The parser @manyMap f p@, will parse @p@ __zero__ or more times, then
adapt each result with the function @f@ to produce a bunch of values
in some 'Monoid' @m@. These values are then combined together to form a
single value; if @p@ could not be parsed, it will return the 'mempty'
for @m@.

==== __Examples__
>>> parse (manyMap Set.singleton item) "aaaab"
Success (Set.fromList ['a', 'b'])

@since 0.2.2.0
-}
manyMap :: Monoid m
        => (a -> m) -- injection function for parser results into a monoid
        -> Parsec a -- parser to execute multiple times
        -> Parsec m
manyMap f = manyr (<>) mempty . fmap f

{-|
This combinator acts like the 'foldMap' function but with a parser.

The parser @manyMap f p@, will parse @p@ __one__ or more times, then
adapt each result with the function @f@ to produce a bunch of values
in some 'Semigroup' @s@. These values are then combined together to form a
single value.

==== __Examples__
>>> parse (someMap Max item) "bdcjb"
Success (Max 'j')
>>> parse (someMap Min item) "bdcjb"
Success (Max 'b')

@since 0.2.2.0
-}
someMap :: Semigroup s
        => (a -> s) -- injection function for parser results into a monoid
        -> Parsec a -- parser to execute multiple times
        -> Parsec s
someMap f p = _repl (<>) (f <$> p) (f <$> p) -- is there a better implementation, it's tricky!

_repl :: (b -> a -> b) -> Parsec b -> Parsec a -> Parsec b
_repl f k p = k <**> manyr (\x next !acc -> next (f acc x)) id p

-- should these be implemented with branch? probably not.
{-|
This combinator filters the result of this parser using a given predicate, succeeding only if the predicate returns @True@.

First, parse this parser. If it succeeds then take its result @x@ and apply it to the predicate @pred@. If @pred x@ is
true, then return @x@. Otherwise, the combinator fails.

==== __Examples__
>>> keywords = Set.fromList ["if", "then", "else"]
>>> identifier = filterS (\v -> not (Set.member v keywords)) (some letter)
>>> parse identifier "hello"
Success "hello"
>>> parse identifier "if"
Failure ..

@since 0.2.2.0
-}
filterS :: (a -> Bool) -- ^ the predicate that is tested against the parser result.
        -> Parsec a    -- ^ the parser to filter, @p@.
        -> Parsec a    -- ^ a parser that returns the result of @p@ if it passes the predicate.
filterS = filterSWith vanillaGen

-- this is called mapFilter in Scala... there is no collect counterpart
{-|
This combinator applies a function @f@ to the result of this parser: if it returns a
@Just y@, @y@ is returned, otherwise the parser fails.

First, parse this parser. If it succeeds, apply the function @f@ to the result @x@. If
@f x@ returns @Just y@, return @y@. If @f x@ returns @Nothing@, or this parser failed
then this combinator fails. Is a more efficient way of performing a @filterS@ and @fmap@
at the same time.

==== __Examples__
>>> int = ...
>>> safeDiv = mapMaybeS (\(x, y) -> if y /= 0 then Just (div x y) else Nothing) (int <~> (char ' ' *> int))
>>> parse safeDiv "7 0"
Failure .. -- y cannot be 0!
>>> parse safeDiv "10 2"
Success 5

@since 0.2.2.0
-}
mapMaybeS :: (a -> Maybe b) -- ^ the function used to both filter the result of this parser and transform it.
          -> Parsec a       -- ^ the parser to filter, @p@.
          -> Parsec b       -- ^ a parser that returns the result of @p@ applied to @f@, if it yields a value.
mapMaybeS = mapMaybeSWith vanillaGen
