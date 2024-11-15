{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric, CPP #-}
{-|
Module      : Text.Gigaparsec
Description : Contains the bulk of the core combinators.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains the core combinators and parser type: all parsers will require something from
within!

@since 0.1.0.0
-}
module Text.Gigaparsec (
  -- * The 'Parsec' type
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
    pure, empty,
  -- * Result Changing Combinators
  -- | These combinators change the result of the parser they are called on into a value of a
  -- different type. This new result value may or may not be derived from the previous result.
    ($>),
    (<$>), (<$), void,
  -- * Sequencing Combinators
  -- | These combinators all combine two parsers in sequence. The first argument of the combinator
  -- will be executed first, then the second argument second. The results of both parsers are
  -- combined in some way (depending on the individual combinator). If one of the parsers fails, the
  -- combinator as a whole fails.
    (<~>), (<:>),
    (<*>), liftA2, (*>), (<*), (<**>),
  -- * Branching Combinators
  -- | These combinators allow for parsing one alternative or another. All of these combinators are
  -- /left-biased/, which means that the left-hand side of the combinator is tried first: the
  -- right-hand side of the combinator will only be tried when the left-hand one failed (and did not
  -- consume input in the process).
    (<+>),
    (<|>), 
  -- * Selective Combinators
  -- | These combinators will decide which branch to take next based on the result of another parser.
  -- This differs from combinators like `(<|>)` which make decisions based on the success/failure of
  -- a parser: here the result of a /successful/ parse will direct which option is done.
    select, branch,

  -- * Filtering Combinators
  -- | These combinators perform filtering on the results of a parser. This means that, given the
  -- result of a parser, they will perform some function on that result, and the success of that
  -- function effects whether or not the parser fails.
    filterS, mapMaybeS,

  -- * Folding Combinators
  {-| These combinators repeatedly execute a parser (at least zero or one times depending on the
  -- specific combinator) until it fails. The results of the successes are then combined together
  -- using a folding function. An initial value for the accumulation may be given (for the folds),
  -- or the first successful result is the initial accumulator (for the reduces). These are
  -- implemented efficiently and do not need to construct any intermediate list with which to store
  -- the results.
  -}
  -- ** The 'many' Combinators
  {-|
    The 'many' combinators will repeatedly parse a given parser zero or more times, and collect each result in a list.
  -}
  many, manyl, manyr, manyMap,
  -- ** The 'some' Combinators
  {-|
    The 'some' combinators will repeatedly parse a given parser one or more times, and collect each result in a list.
    If successful, the list returned by these combinators is always non-empty.
    
    /See also:/ "Text.Gigaparsec.Combinator.NonEmpty" for variants of these combinators that return a 'Data.List.NonEmpty.NonEmpty' list.
  -}
    some, somel, somer, someMap,-- TODO: these need to be properly categorised
  ) where

-- NOTE:
-- This module is mostly just for re-exports, though there may be primitive
-- combinators in here too. If users can see it, it can go in here.
--
-- Care MUST be taken to not expose /any/ implementation details from
-- `Internal`: when they are in the public API, we are locked into them!

import Text.Gigaparsec.Internal (Parsec(Parsec), emptyState, manyr, somer)
import Text.Gigaparsec.Internal qualified as Internal (State(..), useHints, expectedErr)
import Text.Gigaparsec.Internal.Errors qualified as Internal (Error, ExpectItem(ExpectEndOfInput), fromError)

import Text.Gigaparsec.Errors.ErrorBuilder (ErrorBuilder)
import Text.Gigaparsec.Errors.Combinator (filterSWith, mapMaybeSWith)
import Text.Gigaparsec.Errors.ErrorGen (vanillaGen)


-- We want to improve the docs for Applicative, so we do some trickery with redefinitions 
--  *only* for the haddock generation.
#ifdef __HADDOCK__
{-# LANGUAGE NoImplicitPrelude #-}
import Prelude hiding ((<$>), (<$), (<*>), (*>), (<*), pure)
import Prelude qualified
import Control.Applicative qualified as Applicative (liftA2, (<|>), empty, many, some, (<**>), (<*>), (*>), (<*), pure) 
import Control.Applicative (Alternative)
import Control.Selective qualified as Applicative (select, branch)
import Data.Functor qualified (void)
#else
import Data.Functor (void)
import Control.Applicative (liftA2, (<|>), empty, many, some, (<**>)) -- liftA2 required until 9.6
import Control.Selective (select, branch)
#endif

import Control.Monad.RT (RT, runRT, rtToIO)

import Data.Set qualified as Set (singleton, empty)
import GHC.Generics (Generic)


-- Hiding the Internal module seems like the better bet: nobody needs to see it anyway :)
-- re-expose like this to prevent hlint suggesting import refinement into internal
--type Parsec :: * -> *
--type Parsec = Internal.Parsec

{-|
Similar to @Either@, this type represents whether a parser has failed or not.

This is chosen instead of @Either@ to be more specific about the naming.

@since 0.1.0.0
-}
{- 
TODO: could we instead just use
> type Result = Either
and pattern synonyms,
> pattern Success :: a -> Result e a
> pattern Success x = Right x
> pattern Failure :: e -> Result e a
> pattern Failure err = Left err
-}
type Result :: * -> * -> *
data Result e a = 
  -- | The parser succeeded with a result of type @a@.
    Success a 
  -- | The parser failed with an error of type @e@.
  | Failure e deriving stock (Show, Eq, Generic) 
  -- TODO: monad?

{-|
A fold for the 'Result' type.

This functions like the 'either' function does for 'Either'.

@since 0.2.0.0
-}
result :: (e -> b)   -- ^ @failure@, the function that handles the failure case.
       -> (a -> b)   -- ^ @success@, the function that handles the successful case.
       -> Result e a -- ^ @result@, the parse result to process.
       -> b          -- ^ applies either @failure@ or @success@ to the @result@, depending on
                     -- whether or not it is successful.
result _ success (Success x) = success x
result failure _ (Failure err) = failure err

{-|
Runs a parser against some input.

Given a parser, some input, and a way of producing parse errors of a desired
type (via 'ErrorBuilder'), this function runs a parser to produce either a
successful result or an error. Note that the @err@ type parameter is first,
which allows for @parse \@String@ to make use of the defaultly formated @String@
error messages. This may not be required if it is clear from context. To make
this process nicer within GHCi, consider using 'parseRepl'.

@since 0.2.0.0
-}
{-# SPECIALISE parse :: Parsec a -> String -> Result String a #-}
{-# INLINABLE parse #-}
parse :: forall err a. ErrorBuilder err
      => Parsec a     -- ^ the parser to execute
      -> String       -- ^ the input to parse
      -> Result err a -- ^ result of the parse, either an error or result
parse p inp = runRT $ _parse Nothing p inp

{-|
Runs a parser against some input, pretty-printing the result to the terminal.

Compared, with 'parse', this function will always generate error messages as
strings and will print them nicely to the terminal (on multiple lines). If the
parser succeeds, the result will also be printed to the terminal. This is useful
for playing around with parsers in GHCi, seeing the results more clearly.

@since 0.2.0.0
-}
parseRepl :: Show a
          => Parsec a -- ^ @p@, the parser to execute.
          -> String   -- ^ @input@, the input to parse.
          -> IO ()    -- ^ An 'IO' action which parses @input@ with @p@, 
                      -- and prints the result to the terminal.
parseRepl p inp =
  do res <- rtToIO $ _parse Nothing p inp
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
     rtToIO $ _parse (Just f) p inp

--TODO: parseFromHandle?

{-# INLINE _parse #-}
_parse :: forall err a. ErrorBuilder err => Maybe FilePath -> Parsec a -> String -> RT (Result err a)
_parse file (Parsec p) inp = p (emptyState inp) good bad
  where good :: a -> Internal.State -> RT (Result err a)
        good x _  = return (Success x)
        bad :: Internal.Error -> Internal.State -> RT (Result err a)
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
This combinator, pronounced "zip", first parses @p@ then parses @q@: 
if both succeed the result of @p@ is paired with that of @q@.

First, runs @p@, yielding @x@ on success, then runs @q@, yielding @y@ on success. 
If both are successful then @(x, y)@ is returned. 
If either fail then the entire combinator fails.

==== __Examples__
>>> p = char 'a' <~> char 'b'
>>> parse p "ab"
Success ('a', 'b')
>>> parse p "b"
Failure ..
>>> parse p "a"
Failure ..

@since 0.1.0.0
-}
infixl 4 <~>
(<~>) :: Parsec a      -- ^ @p@, the first parser to run.
      -> Parsec b      -- ^ @q@, the second parser to run.
      -> Parsec (a, b) -- ^ a parser that runs @p@ and then @q@, and pairs their results.
(<~>) = liftA2 (,)

{-|
This combinator, pronounced "as", replaces the result of the given parser, ignoring the old result.

Similar to @(<$>)@, except the old result of the given parser is not required to
compute the new result. This is useful when the result is a constant value (or function!).
Functionally the same as @p *> pure x@ or @const x <$> p@.

/In [scala parsley](https:\/\/j-mie6.github.io\/parsley\/), this combinator is known as /@#>@/ or /'@as@'.

==== __Examples__
>>> parse (string "true" $> true) "true"
Success true

@since 0.1.0.0
-}
infixl 4 $>
($>) :: Parsec a -- ^ @p@, the parser to run, whose result is ignored.
     -> b        -- ^ @x@, the value to return
     -> Parsec b -- ^ a parser that runs @p@, but returns the value @x@.
($>) = flip (<$)

{-|
This combinator, pronounced "cons", first parses @p@ and then @ps@: if both succeed the result of this
parser is prepended onto the result of @ps@.

First, runs @p@, yielding @x@ on success, then runs @ps@, yielding @xs@ on success. 
If both are successful then @x : xs@ is returned. 
If either fail then the entire combinator fails.

==== __Examples__
> some p = p <:> many(p)

@since 0.1.0.0
-}
infixl 4 <:>
(<:>) :: Parsec a   -- ^ @p@, the first parser to run
      -> Parsec [a] -- ^ @q@, the second parser to run
      -> Parsec [a] -- ^ a parser that runs @p@ and then @q@, 
                    -- and prepends the result of the former onto that of the latter.
(<:>) = liftA2 (:)

infixl 3 <+>
{-|
This combinator, pronounced "sum", wraps the given parser's result in @Left@ if it succeeds, and parses @q@ if it failed __without__ consuming input,
wrapping the result in @Right@.

If the given parser @p@ is successful, then its result is wrapped up using @Left@ and no further action is taken.
Otherwise, if @p@ fails __without__ consuming input, then @q@ is parsed instead and its result is
wrapped up using @Right@.
If @p@ fails having consumed input, this combinator fails.
This is functionally equivalent to @Left <$> p <|> Right <$> q@.

The reason for this behaviour is that it prevents space leaks and improves error messages. 
If this behaviour is not desired, use @atomic p@ to rollback any input consumed on failure.

==== __Examples__
>>> p = string "abc" <+> char "xyz"
>>> parse p "abc"
Success (Left "abc")
>>> parse p "xyz"
Success (Right "xyz")
>>> parse p "ab"
Failure .. -- first parser consumed an 'a'!

@since 0.1.0.0
-}
(<+>) :: Parsec a            -- ^ @p@, the first parser to run
      -> Parsec b            -- ^ @q@, the parser to run if @p@ fails without consuming input
      -> Parsec (Either a b) -- ^ a parser which either parses @p@, or @q@, projecting their 
                             -- results into an 'Either'.
p <+> q = Left <$> p <|> Right <$> q

{-|
This combinator will parse the given parser __zero__ or more times combining the results with the function @f@ and base value @k@ from the left.

The given parser @p@ will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a left-to-right
fashion using the function @f@: the left-most value provided to @f@ is the value @k@.

If @p@ does fail at any point having consumed input, this combinator will fail.

@since 0.3.0.0
-}
manyl :: (b -> a -> b) -- ^ @f@, function to apply to each value produced by @p@ starting at the left.
      -> b             -- ^ @k@, the initial value to feed into the reduction
      -> Parsec a      -- ^ @p@, the parser to repeatedly run zero or more times.
      -> Parsec b      -- ^ a parser which parses @p@ many times and folds the results together with @f@ and @k@ left-associatively.
manyl f k = _repl f (pure k)

{-|
This combinator will parse the given parser __one__ or more times combining the results with the function @f@ and base value @k@ from the left.

The given parser @p@ will continue to be parsed until it fails having __not consumed__ input.
All of the results generated by the successful parses are then combined in a left-to-right
fashion using the function @f@: the left-most value provided to @f@ is the value @k@.
If @p@ fails at any point having consumed input, this combinator fails.

==== __Examples__
> natural = somel (\x d -> x * 10 + digitToInt d) 0 digit

@since 0.3.0.0
-}
somel :: (b -> a -> b) -- ^ @f@, the function to apply to each value produced by @p@, starting from the left.
      -> b             -- ^ @k@, the initial value to feed into the reduction.
      -> Parsec a      -- ^ @p@ the parser to run at least once.
      -> Parsec b      -- ^ a parser which parses @p@ at least once, and folds the results together with @f@ and @k@ left-associatively.
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
        => (a -> m) -- ^ @f@, injection function for parser results into a monoid
        -> Parsec a -- ^ @p@, parser to execute multiple times
        -> Parsec m -- ^ a parser that repeatedly parses @p@, converting each result to an @s@ with @f@,
                    -- ^ collecting the results together using '<>'.
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
        => (a -> s) -- ^ @f@, the injection function for parser results into a monoid
        -> Parsec a -- ^ @p@, the parser to execute multiple times
        -> Parsec s -- ^ a parser that parses @p@ at least once, converting each result to an @s@ with @f@,
                    -- ^ collecting the results together using '<>'.
someMap f p = _repl (<>) (f <$> p) (f <$> p) -- is there a better implementation, it's tricky!

_repl :: (b -> a -> b) -> Parsec b -> Parsec a -> Parsec b
_repl f k p = k <**> manyr (\x next !acc -> next (f acc x)) id p

-- should these be implemented with branch? probably not.
{-|
This combinator filters the result of the given parser @p@ using the given predicate, 
succeeding only if the predicate returns @True@.

First, parse @p@. 
If it succeeds then take its result @x@ and apply it to the predicate @pred@. 
If @pred x@ is true, then return @x@. 
Otherwise, the combinator fails.

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
This combinator applies a function @f@ to the result of the given parser @p@: 
if it returns a @Just y@, @y@ is returned, otherwise this combinator fails.

First, parse @p@. If it succeeds, apply the function @f@ to the result @x@. If
@f x@ returns @Just y@, return @y@. If @f x@ returns @Nothing@, or @p@ failed
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
mapMaybeS :: (a -> Maybe b) -- ^ the function used to both filter the result of @p@ and transform it.
          -> Parsec a       -- ^ the parser to filter, @p@.
          -> Parsec b       -- ^ a parser that returns the result of @p@ applied to @f@, if it yields a value.
mapMaybeS = mapMaybeSWith vanillaGen

---------------------------------------------------------------------------------------------------
-- Haddock specific re-exports
{-
The haddock documentation for re-exports from other libraries is sometimes quite vague/confusing.
We use some C++ macro stuff to only redefine these re-exports for the gigaparsec haddock documentation,
which lets us provide more descriptive (and specialised) documentation for combinators.
-}

#ifdef __HADDOCK__
{-|
Repeatedly parse the given parser __zero__ or more times, collecting the results into a list.

Parses the given parser @p@ repeatedly until it fails. 
If @p@ failed having consumed input, this combinator fails. 
Otherwise, when @p@ fails __without consuming input__, this combinator will return all of the results, 
@x₁@ through @xₙ@ (with @n@ >= 0), in a 'List': @[x₁, .., xₙ]@. 
If @p@ was never successful, the empty 'List' is returned.

/Note:/ This is a re-export of [Control.Applicative.many]("Control.Applicative").
If you use 'many' from this module, it actually has the more general type as found in [Control.Applicative.many]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.some]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
many :: Parsec a   -- ^ @p@, the parser to execute multiple times.
     -> Parsec [a] -- ^ a parser that parses @p@ until it fails, 
                   -- returning the list of all the successful results.
many = Applicative.many

{-|
Repeatedly parse the given parser __one__ or more times, collecting the results into a list.

Parses the given parser @p@ repeatedly until it fails. 
If @p@ failed having consumed input, this combinator fails. 
Otherwise, when @p@ fails __without consuming input__, this combinator will return all of the results, 
@x₁@ through @xₙ@ (with @n@ >= 1), in a 'List': @[x₁, .., xₙ]@. 
If @p@ was not successful at least one time, this combinator fails.

/See Also:/ 'Text.Gigaparsec.Combinator.NonEmpty.some' for a version of this combinator which 
returns a 'Data.List.NonEmpty' list of results. 

/Note:/ This is a re-export of [Control.Applicative.some]("Control.Applicative").
If you use 'some' from this module, it actually has the more general type as found in [Control.Applicative.some]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.some]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
some :: Parsec a   -- ^ @p@, the parser to execute multiple times.
     -> Parsec [a] -- ^ a parser that parses @p@ until it fails, 
                   -- returning the list of all the successful results.
some = Applicative.some

{-|
Given the parsers @p@ and @q@, this combinator parses @p@; 
if @p@ fails __without consuming input__, it then parses @q@.

If @p@ is successful, then this combinator is successful and no further action is taken. 
Otherwise, if @p@ fails without consuming input, then @q@ is parsed instead. 
If @p@ (or @q@) fails having consumed input, this combinator fails.

This behaviour prevents space leaks and improves error messages. 
If this is not desired, use @atomic p@ to rollback any input consumed on failure in the first branch.

This combinator is /associative/, meaning that:

prop> (p <|> q) <|> r = p <|> (q <|> r)

/Note:/ This is a re-export of [Control.Applicative.<|>]("Control.Applicative").
If you use '<|>' from this module, it actually has the more general type as found in [Control.Applicative.<|>]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.<|>]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
-- TODO: re-structure gigaparsec (perhaps a Gigaparsec.Gigaparsec module) that is used throughout the library (as it has the more general type),
-- and only expose '<|>' in this top level module so that it has nice user-facing docs.
infixl 3 <|>
(<|>) :: Parsec a -- ^ @p@ the first parser to run
      -> Parsec a -- ^ @q@ the second parser to run if @p@ fails without consuming input.
      -> Parsec a -- ^ a parser which parses either @p@ or @q@.
(<|>) = (Applicative.<|>)


{-|
Given parsers @p@ and @f@, this combinator, pronounced "reverse ap", first parses @p@ then parses @f@: 
if both succeed then returns the result of @f@ to that of @p@.

First, runs @p@, yielding a value x on success, then @f@ is ran, yielding a function @g@ on success. 
If both are successful, then @g(x)@ is returned. 
If either fail then the entire combinator fails.

Compared with '<*>', this combinator is useful for left-factoring: 
when two branches of a parser share a common prefix, this can often be factored out; 
but the result of that factored prefix may be required to help generate the results of each branch. 
In this case, the branches can return functions that, when given the factored result, 
can produce the original results from before the factoring.

/Note:/ This is a re-export of [Control.Applicative.\<**\>]("Control.Applicative").
If you use '<**>' from this module, it actually has the more general type as found in [Control.Applicative.\<**\>]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.\<**\>]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 <**>
(<**>) :: Parsec a        -- ^ @p@, the first parser to run
       -> Parsec (a -> b) -- ^ @q@, the second parser to run
       -> Parsec b        -- ^ a parser that runs @p@ and then @q@, and applies the result of @q@ to that of @p@
(<**>) = (Applicative.<**>)

{-|
This parser fails immediately, with an unknown parse error.

Although this parser has type @'Parsec' a@, no @a@ will actually be returned, as this parser fails.

This is the \'zero\' or \'identity\' of '<|>':

prop> p <|> empty = empty <|> p = p

/Note:/ This is a re-export of [Control.Applicative.empty]("Control.Applicative").
If you use 'empty' from this module, it actually has the more general type as found in [Control.Applicative.empty]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.empty]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
empty :: Parsec a -- ^ a parser that immediately fails.
empty = Applicative.empty

{-|
This combinator applies the given parsers in sequence and then applies the given function f of to all of the results.

First, parse @p@, then parse @q@.
If both parsers succeed, this combinator succeeds, returning the application of @f@ to the results of @p@ and @q@.
If either @p@ or @q@ fails, this combinator fails.

/Note:/ This is a re-export of [Control.Applicative.liftA2]("Control.Applicative").
If you use 'liftA2' from this module, it actually has the more general type as found in [Control.Applicative.liftA2]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.liftA2]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
liftA2 :: (a -> b -> c) -- ^ @f@, a function to apply to the results of the parsers @p@ and @q@ 
       -> Parsec a -- ^ @p@, the first parser to run
       -> Parsec b -- ^ @q@, the second parser to run
       -> Parsec c -- ^ a parser that parsers @p@, then @q@, and then combines their results with @f@.
liftA2 = Applicative.liftA2

{-|
This combinator parses its first argument @p@, then parses @q@ only if @p@ returns a Left.

First, parse @p@, which, if successful, will produce either a @Left x@ or a @Right y@:
  
* If a @Left x@ is produced, then the parser @q@ is executed to produce a function @f@, and @f x@ is returned. 
* Otherwise, if a Right(y) is produced, y is returned unmodified and q is not parsed. 
  
If either @p@ or @q@ fails, the entire combinator fails. 
This is a special case of 'branch' where the right branch is @pure id@.

/Note:/ This is a re-export of [Control.Selective.select]("Control.Selective").
If you use 'select' from this module, it actually has the more general type as found in [Control.Selective.select]("Control.Selective")
(it works for any 'Control.Selective.Selective' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Selective.select]("Control.Selective"), 
as this has the more general type.

@since 0.1.0.0
-}
select :: Parsec (Either a b) -- ^ @p@, the first parser to execute
       -> Parsec (a -> b)     -- ^ @q@, the parser to to execute when @p@ returns a @Left@
       -> Parsec b            -- ^ a parser that will parse @p@ then possibly parse @q@ to 
                              -- transform @p@'s result into a @b@.
select = Applicative.select

{-|
This combinator parses its first argument @either@, and then parses either @left@ or @right@
depending on its result.

First, parses @either@, which, if successful, produces either a @Left x@ or a @Right y@:

* If a @Left x@ is produced, run @left@ is to produce a function @f@, and return @f x@. 
* If a @Right y@ is produced, run @right@ to produce a function @g@, and return @g y@. 

If either of the two executed parsers fail, the entire combinator fails.

/Note:/ This is a re-export of [Control.Selective.branch]("Control.Selective").
If you use 'branch' from this module, it actually has the more general type as found in [Control.Selective.branch]("Control.Selective")
(it works for any 'Control.Selective.Selective' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Selective.branch]("Control.Selective"), 
as this has the more general type.

@since 0.1.0.0
-}
branch :: Parsec (Either a b) -- ^ @either@, the first parser to run; its result decides which parser to execute next.
       -> Parsec (a -> c)     -- ^ @left@, the parser to run if @either@ returns a @Left@.
       -> Parsec (b -> c)     -- ^ @right@, the parser to run if @either@ returns a @Right@.
       -> Parsec c            -- ^ a parser that will parse one of @left@ or @right@ depending on @either@'s result.
branch = Applicative.branch

{-|
This combinator applies a function @f@ to the result of a parser @p@.

When @p@ succeeds with value @x@, return @f x@.

This can be used to build more complex results from parsers, 
instead of just characters or strings.

/Note:/ This is a re-export of [Data.Functor.\<$\>]("Data.Functor").
If you use '<$>' from this module, it actually has the more general type as found in [Data.Functor.\<$\>]("Data.Functor")
(it works for any 'Functor' @f@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Data.Functor.\<$\>]("Data.Functor") (or the 'Prelude'), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 <$>
(<$>) :: (a -> b) -- ^ @f@, the function to apply to the result of the parser @p@
      -> Parsec a -- ^ @p@, the parser whose result on which to apply @f@.
      -> Parsec b -- ^ a new parser that behaves the same as @p@, but with the given function @f@ applied to its result.
(<$>) = (Prelude.<$>)

{-|
This combinator runs the given parser @p@, and ignores its results, instead returning the given value @x@.

Similar to '<$>', except the result of @p@ is not required to compute the result.
This may be defined in terms of '<$>':

prop> x <$ p = const x <$> p

/Note:/ This is a re-export of [Data.Functor.<$]("Data.Functor").
If you use '<$' from this module, it actually has the more general type as found in [Data.Functor.<$]("Data.Functor")
(it works for any 'Functor' @f@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Data.Functor.<$]("Data.Functor") (or the 'Prelude'), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 <$
(<$) :: a -- ^ @x@, the value to return after parsing @p@.
     -> Parsec b -- ^ @p@, the parser to run and then discard its results.
     -> Parsec a -- ^ a parser that parses @p@, discards its results, and returns @x@.
(<$) = (Prelude.<$)

{-|
This combinator produces a value without having any other effect.

When this combinator is run, no input is required, nor consumed, 
and the given value will always be successfully returned. 
It has no other effect on the state of the parser.

/Note:/ This is a re-export of [Control.Applicative.pure]("Control.Applicative").
If you use 'pure' from this module, it actually has the more general type as found in [Control.Applicative.pure]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.pure]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
-- TODO: re-structure gigaparsec (perhaps a Gigaparsec.Gigaparsec module) that is used throughout the library (as it has the more general type),
-- and only expose 'pure' in this top level module so that it has nice user-facing docs.
pure :: a        -- ^ @x@ the value to be returned.
     -> Parsec a -- ^ a parser which consumes no input and produces the value @x@.
pure = (Applicative.pure)

{-|
This combinator, pronounced "ap", first parses @p@ then parses @q@: 
if both succeed then the function @p@ returns is applied to the value returned by @q@.

First, runs @p@, yielding a function @f@ on success, then runs @q@, yielding a value @x@ on success. 
If both @p@ and @q@ are successful, then returns @f x@. 
If either fail then the entire combinator fails.

/Note:/ This is a re-export of [Control.Applicative.\<*\>]("Control.Applicative").
If you use '<*>' from this module, it actually has the more general type as found in [Control.Applicative.\<*\>]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.\<*\>]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 <*>
(<*>) :: Parsec (a -> b) -- ^ @p@, a parser that produces a function @f@.
      -> Parsec a -- ^ @q@, the parser to run second, which returns a value applicable to @p@'s result. 
      -> Parsec b -- ^ a parser that runs @p@ and then @q@ and combines their results with function application.
(<*>) = (Applicative.<*>)

{-|
This combinator, pronounced "then", first parses @p@ then parses @q@: 
if both @p@ and @q@ succeed then the result of @q@ is returned.

First, runs @p@, yielding @x@ on success, then runs @q@, yielding @y@ on success. 
If both @p@ and @q@ are successful then returns @y@ and ignores @x@ 
If either fail then the entire combinator fails.

/Note:/ This is a re-export of [Control.Applicative.*>]("Control.Applicative").
If you use '*>' from this module, it actually has the more general type as found in [Control.Applicative.*>]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.*>]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 *>
(*>) :: Parsec a -- ^ @p@, the first parser to run, whose result is discarded.
     -> Parsec b -- ^ @q@, the second parser to run, which returns the result of this combinator.
     -> Parsec b -- ^ a parser that runs @p@ then @q@ and returns @q@'s result.
(*>) = (Applicative.*>)

{-|
This combinator, pronounced "then discard", first parses @p@ then parses @q@: 
if both succeed then the result of @p@ is returned.

First, runs @p@, yielding @x@ on success, then runs @q@, yielding @y@ on success. 
If both @p@ and @q@ are successful then returns @x@ and ignores @y@. 
If either fail then the entire combinator fails.

/Note:/ This is a re-export of [Control.Applicative.<*]("Control.Applicative").
If you use '<*' from this module, it actually has the more general type as found in [Control.Applicative.<*]("Control.Applicative")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Control.Applicative.<*]("Control.Applicative"), 
as this has the more general type.

@since 0.1.0.0
-}
infixl 4 <*
(<*) :: Parsec a -- ^ @p@, the first parser to run, which returns the result of this combinator.
     -> Parsec b -- ^ @q@, the second parser to run, whose result is discarded.
     -> Parsec a -- ^ a parser that runs @p@ then @q@ and returns @p@'s result.
(<*) = (Applicative.<*)

{-|
Run the given parser @p@, then discard its result.

This combinator is useful when @p@ should be run, but its result is not required.

/Note:/ This is a re-export of [Data.Functor.void]("Data.Functor").
If you use 'void' from this module, it actually has the more general type as found in [Data.Functor.void]("Data.Functor")
(it works for any 'Control.Applicative.Applicative' @p@, rather than just 'Parsec').

/For Gigaparsec Developers:/ Do not import this, use [Data.Functor.void]("Data.Functor"), 
as this has the more general type.

@since 0.1.0.0
-}
void :: Parsec a  -- ^ @p@, the parser we wish to run, but whose results are unwanted.
     -> Parsec () -- ^ a parser that behaves the same as @p@, but returns '()' on success.
void = Data.Functor.void
#endif


