{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude, PatternSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-|
Module      : Text.Gigaparsec.Token.Indentation
Description : This module defines combinators for handling indentation-sensitive grammars.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module defines some core combinators for handling indentation-sensitive grammars, 
including the handling of line-folds and indented blocks.

@since 0.4.0.0

-}
module Text.Gigaparsec.Token.Indentation (
  -- * Indentation-Sensitive Parser Constructors
  nonIndented,
  indentMany,
  indent,
  indentSome,
  indent1,
  -- ** Fixed Indentation
  indentManyAt,
  indentSomeAt,

  -- ** Indent-After
  -- *** Many
  indentManyAfter,
  indentAfter,
  thenIndent,
  -- *** Some
  indentSomeAfter,
  indentAfter1,
  thenIndent1,
  -- * Line-Folds
  -- $lineFolding
  lineFold,
  lineFoldWs,

  -- * Megaparsec-style Indentation Blocks.
  {-|
  The combinators here should be more familiar to users of [megaparsec](https://hackage.haskell.org/package/megaparsec).
  They can all be emulated by the combinators above.
  -}
  IndentOpt(..),
  indentBlock,
  indentGuard,
  lineFoldMegaparsec
  ) where

import Prelude hiding (fail)
import Data.List.NonEmpty (NonEmpty)
import Text.Gigaparsec (Parsec, atomic, unit)
import Text.Gigaparsec.Char (endOfLine, newline)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative ((<|>), many))
import Control.Monad (unless, void)
import Text.Gigaparsec.Combinator.NonEmpty (some)

import Text.Gigaparsec.Internal.Token.Indentation (
  indentLevel,
  nonIndented, IndentLevel
  )
import Text.Gigaparsec.Internal.Token.Indentation qualified as Internal
import Text.Gigaparsec.Token.Lexer (Space)
import Text.Gigaparsec.Internal.Token.Lexer (Space, whiteSpace)
import Text.Gigaparsec.Combinator (skipMany)
import Text.Gigaparsec.Internal.Token.Space (Space(_indentGuard))
import Text.Gigaparsec.State (make)


{- $lineFolding
This description of line-folds is based on that given by [Karpov](https://markkarpov.com/tutorial/megaparsec.html#adding-line-folds).

A /line-fold/ is an expression consisting of many elements that may be on the same line, 
or spread across multiple lines, where each subsequent line may be at any indentation greater than that of the first.
As each subsequent line may be at *different* indentations, line-folds are subtly distinct from indentation blocks,
in which each line must be at the *same* indentation.

An example of a language featuring line-folding is Haskell :)

This module provides three combinators for line-folding, use whichever you prefer:

* 'lineFold'
* 'lineFoldWs'
* 'lineFoldMegaparsec'

==== __Example__

In the presence of line-folding, each of these programs should have the same meaning:
> foo bar baz

> foo
>   bar  baz

> foo
>    bar
>  baz

And the following programs do /not/ have the same meaning as @foo bar baz@:

> foo
> bar
> baz

and,

> foo
>   bar
> baz

We may choose from any of the line-folding combinators to create a parser capable 
of handling these expressions.

===== With 'lineFold'

> symbol :: String -> Parsec ()

'lineFold' is probably the easiest combinator to work with.
> myFold = lineFold $ do
>   symbol "foo"
>   symbol "bar"
>   symbol "baz"

===== With 'lineFoldWs'

'lineFoldWs' is similar to `lineFoldMegaparsec`, except it assumes the given whitespace parser
*does not* consume newlines.

> -- symbol here is parameterised by a whitespace parser
> symbol :: Parsec () -> String -> Parsec ()
> -- ws *must not* consume newlines.
> ws :: Parsec ()
>
> myFold = lineFoldWs ws $ \ws' -> do
>   symbol ws' "foo"
>   symbol ws' "bar"
>   symbol ws "baz"

===== Megaparsec-style

Again, this is lifted from [Karpov](https://markkarpov.com/tutorial/megaparsec.html#adding-line-folds).

> -- symbol here is parameterised by a whitespace parser
> symbol :: Parsec () -> String -> Parsec ()
> -- ws *must* consume newlines.
> ws :: Parsec ()
>
> myFold = L.lineFold ws $ \ws' -> do
>   L.symbol ws' "foo"
>   L.symbol ws' "bar"
>   L.symbol ws  "baz" -- for the last symbol we use the normal space consumer

-}

{-|
Parse zero or more items, each at the same indentation level as the first item parsed.

@`indentMany` ws p@ first consumes any initial whitespace (with @ws@) and newlines,
and then parses a @p@ at some indentation @x@.
Then parses any newlines and whitespace.
This repeats for @p@s that are at the same indentation @x@, 
collecting the results in a list.
If the initial @p@ fails without consuming input, then this combinator succeeds, returning @[]@.

This combinator fails iff any @p@ fails having consumed input.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ does also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentMany #-}
indentMany
  :: forall b
  .  Parsec ()  -- ^ @ws@, whitespace consumer. Does not need to consume newlines.
  -> Parsec b   -- ^ @p@, the indented items to parse.
  -> Parsec [b] -- ^ A parser that parses zero or more @p@s, each at the same indentation, 
                -- collecting their results in a list.
indentMany = Internal.indentMany Nothing

{-|
Parse zero or more items, each at the given indentation level.

@`indentManyAt` lvl ws p@ first consumes any initial whitespace (with @ws@) and newlines,
and then checks that the current indentation is @lvl@.
If so, parses a @p@, then parses any newlines and whitespace.
The indentation check and parsing repeats for @p@s that are at the same indentation @lvl@, 
collecting the results in a list.

If either the first indentation check or the initial @p@ fails without consuming input, 
then this combinator succeeds, returning @[]@.

This combinator fails iff any @p@ fails having consumed input.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ does also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentManyAt #-}
indentManyAt
  :: forall b
  .  IndentLevel  -- ^ @lvl@, the reference indentation level.
  -> Parsec ()    -- ^ @ws@, whitespace consumer. Does not need to consume newlines.
  -> Parsec b     -- ^ @p@, the indented items to parse.
  -> Parsec [b]   -- ^ A parser that parses zero or more @p@s, each at the indentation @lvl@, 
                  -- collecting their results in a list.
indentManyAt = Internal.indentMany . Just


{-|
Parse one or more items, each at the same indentation level as the first item parsed.

@`indentSome` ws p@ first consumes any initial whitespace (with @ws@) and newlines,
and then parses a @p@ at some indentation @x@.
Then parses any newlines and whitespace.
This repeats for @p@s that are at the same indentation @x@, 
collecting the results in a list.

If the initial @p@ fails without consuming input, this combinator fails.
This combinator also fails if any @p@ fails having consumed input.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ does also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentSome #-}
indentSome
  :: forall b
  .  Parsec ()            -- ^ @ws@, whitespace consumer. Does not need to consume newlines.
  -> Parsec b             -- ^ @p@, the indented items to parse.
  -> Parsec (NonEmpty b)  -- ^ A parser that parses at least one @p@, each at the same indentation, 
                          -- collecting their results in a list.
indentSome = Internal.indentSome Nothing

{-|
Parse one or more items, each at the given indentation level.

@`indentSomeAt` lvl ws p@ first consumes any initial whitespace (with @ws@) and newlines,
and then checks that the current indentation is @lvl@.
If so, parses a @p@, then parses any newlines and whitespace.
The indentation check and parsing repeats for @p@s that are at the same indentation @lvl@, 
collecting the results in a list.

If either the first indentation check or the initial @p@ fails without consuming input, 
then this combinator fails.
This combinator also fails if any @p@ fails, having consumed input.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ does also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentSomeAt #-}
indentSomeAt
  :: forall b
  .  IndentLevel         -- ^ @lvl@, the reference indentation level.
  -> Parsec ()           -- ^ @ws@, whitespace consumer. Does not need to consume newlines.
  -> Parsec b            -- ^ @p@, the indented items to parse.
  -> Parsec (NonEmpty b) -- ^ A parser that parses at least one @p@, each at the indentation @lvl@, 
                         -- collecting their results in a list.
indentSomeAt = Internal.indentSome . Just

{-|
Alias for `indentMany`.

@since 0.4.0.0

-}
{-# INLINE indent #-}
indent :: Parsec () -> Parsec b -> Parsec [b]
indent = indentMany

{-|
Alias for `indentSome`.

@since 0.4.0.0

-}
{-# INLINE indent1 #-}
indent1 :: Parsec () -> Parsec b -> Parsec (NonEmpty b)
indent1 = indentSome

---------------------------------------------------------------------------------------------------
-- Indent After

{-|
Parse the first item, then parse zero or more of the second items, each must be at the same indentation
which is greater than that of the first item.

@`indentManyAfter` ws p q@ first consumes any initial whitespace (with @ws@) and newlines,
and then saves the current indentation level, @lvl@ (this will be the indentation of @p@).
Then, parses a @p@, and then parses many @q@s, each must be at the indentation @lvl@.
The results of @p@ and a list of all the results of the @q@s are returned as a pair.

If either the first indentation check or the initial @p@ fails without consuming input, 
then this combinator fails.
This combinator also fails if any @q@ fails, having consumed input.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ does also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentManyAfter #-}
indentManyAfter :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, [b])
indentManyAfter ws p q = do
  refLvl <- indentLevel
  (,) <$> p <*> indentManyAt refLvl ws q

{-|
Alias for `indentManyAfter`.

@since 0.4.0.0

-}
{-# INLINE indentAfter #-}
indentAfter :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, [b])
indentAfter = indentManyAfter

{-|
Alias for `indentAfter`.
Most useful as an infix operator:

> p `thenIndent` q

@since 0.4.0.0

-}
{-# INLINE thenIndent #-}
thenIndent :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, [b])
thenIndent = indentManyAfter

{-|
Parse the first item, then parse one or more of the second items, each must be at the same indentation
which is greater than that of the first item.

@`indentSomeAfter` ws p q@ first consumes any initial whitespace (with @ws@) and newlines,
and then saves the current indentation level, @lvl@ (this will be the indentation of @p@).
Then, parses a @p@, and then parses one or more @q@s, each must be at the indentation @lvl@.
The results of @p@ and a list of all the results of the @q@s are returned as a pair.

If either the first indentation check or the initial @p@ fails without consuming input, 
then this combinator fails.
This combinator also fails if any @q@ fails, having consumed input, or if no @q@ succeeds.

This combinator consumes input iff there are any initial newlines or @ws@, and/or if @p@ and @q@ do also.

/Note:/ This combinator will consume any final newlines at the end of an indentation block.

@since 0.4.0.0

-}
{-# INLINE indentSomeAfter #-}
indentSomeAfter :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, NonEmpty b)
indentSomeAfter ws p q = do
  refLvl <- indentLevel
  (,) <$> p <*> indentSomeAt refLvl ws q

{-|
Alias for `indentSomeAfter`.

@since 0.4.0.0

-}
{-# INLINE indentAfter1 #-}
indentAfter1 :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, NonEmpty b)
indentAfter1 = indentSomeAfter

{-|
Alias for `indentSomeAfter`.
Most useful as an infix operator:

> p `thenIndent1` q

@since 0.4.0.0

-}
{-# INLINE thenIndent1 #-}
thenIndent1 :: Parsec () -> Parsec a -> Parsec b -> Parsec (a, NonEmpty b)
thenIndent1 = indentSomeAfter


---------------------------------------------------------------------------------------------------
-- Line folds 

{-| Run the given parser @p@ as a line-fold, allowing it to be spread across multiple lines,
as long as each subsequent line is more indented than the first.

This parser fails if @p@ does, including if @p@ cannot finish parsing before the end of the indented items.

This parser consumes any initial whitespace, and any input if @p@ does also.

@since 0.4.0.0

-}
lineFold :: Space -> Parsec a -> Parsec a
lineFold sp p = do
  let ws = whiteSpace sp
  refLvl <- ws *> indentLevel
  make refLvl $ \ref -> 
    _indentGuard sp GT ref p <* (newline *> ws)


{-| This is similar to `lineFoldMegaparsec`, except the whitespace parser must *not* consume newlines.

Creates a parser that supports line-folding. 

@'lineFoldWs' ws p@ takes a whitespace parser @ws@, and a parser @p@ parameterised by a whitespace parser 
(i.e. it is a function expecting a whitespace parser, returning a parser).

The whitespace parser @ws@ consumes whitespace between each component of the line fold, 
and it /must not/ consume newlines.
The parser @p@ receives an augmented whitespace parser that checks indentation automatically
and is able to consume newlines,
which @p@ may then use to consume the line fold.

@since 0.4.0.0

-}
lineFoldWs
  :: Parsec () -- ^ @ws@, the whitespace parser. Must not consume newlines.
  -> (Parsec () -> Parsec a) -- ^ @p@, a parser parameterised by a whitespace consumer.
  -> Parsec a
lineFoldWs ws p = do
  refLvl <- ws *> indentLevel
  -- we only need to check indentation after parsing a newline.
  let ws' =  void (some (endOfLine *> ws) *> indentGuard unit GT refLvl) <|> ws
  p ws'


---------------------------------------------------------------------------------------------------
-- Megaparsec Style stuff

{-|
An @`IndentOpt` a b@ contains a parser for @a@s that can be collected to form a @b@.
These describe the behaviour for parsing an indented token with `indentBlock`.

__Note:__ [megaparsec](https://hackage.haskell.org/package/megaparsec) users should be aware this type
is somewhat different from `Text.Megaparsec.Char.Lexer.IndentOpt` in @megaparsec@, 
in which the `IndentSome` and `IndentMany` constructors take a return continuation.
This is not necessary, as we instead use GADTs to index `IndentOpt` by the output type of each constructor.

@since 0.4.0.0

-}
type IndentOpt :: * -> * -> *
data IndentOpt a :: * -> * where
  {-|
  Parse no indented tokens, just return the value.

  @since 0.4.0.0

  -}
  IndentNone :: a -> IndentOpt a a
  {-|
  Parse zero or more indented tokens which start at the given indentation level, collecting their results.

  @`IndentMany` mlvl f p@ uses the given indentation level @mlvl@;
  if @mlvl@ is `Nothing`, then uses the indentation level of the first token.
  Then, repeatedly parses @p@ at the indentation level, once per newline.
  Then, applies @f@ to the results @x₁@, ..., @xₙ@ from each parse of @p@ (where @n@ ≥ 0),
  i.e. @f [x₁, ..., xₙ]@

  @since 0.4.0.0

  -}
  IndentMany
    :: Maybe IndentLevel -- ^ The indentation at which the first token should start.
    -> Parsec a          -- ^ How to parse each indented line.
    -> IndentOpt a [a]
  {-|
  Parse one or more indented tokens which start at the given indentation level, collecting their results.

  @`IndentSome` mlvl f p@ uses the given indentation level @mlvl@;
  if @mlvl@ is `Nothing`, then uses the indentation level of the first token.
  Then, repeatedly parses @p@ at the indentation level, once per newline.
  Then, applies @f@ to the results @x₁@, ..., @xₙ@ from each parse of @p@ (where @n@ ≥ 1),
  i.e. @f [x₁, ..., xₙ]@

  @since 0.4.0.0

  -}
  IndentSome
    :: Maybe IndentLevel      -- ^ The indentation at which the first token should start.
    -> Parsec a               -- ^ How to parse each indented line.
    -> IndentOpt a (NonEmpty a)


{-|
Ensures the given ordering between the actual and the given indentation holds,
returning the former.

@'indentGuard' ws ord refLvl@ will parse any initial whitespace using @ws@, 
and will compare @refLvl@ with the current indentation.

* If this comparison is the same as @ord@, this parser succeeds, returning the new indentation.
* Otherwise, this combinator fails.

This combinator consumes input if and only if there is any initial whitespace.

@since 0.4.0.0

-}
{-# INLINE indentGuard #-}
indentGuard
  :: Parsec ()          -- ^ How to consume whitespace.
  -> Ordering           -- ^ How the actual indentation should be compared with the reference indentation.
  -> IndentLevel        -- ^ The reference indentation with which the actual indentation should be compared.
  -> Parsec IndentLevel -- ^ The actual indentation level.
indentGuard ws ord refLvl = do
  actLvl <- ws *> indentLevel
  unless (compare actLvl refLvl == ord) $
    Internal.throwIndentationError (Internal.ErrIndentNotOrd ord refLvl actLvl)
  return actLvl

{-|
Parse a 'reference' token @ref@, and then parse a series of items that must all be at the same indentation,
which itself must be greater than that of the reference token.

The indented items are parsed using the parser in the given `IndentOpt`.

@since 0.4.0.0


-}
{-# INLINABLE indentBlock #-}
indentBlock
  :: Parsec () -- ^ Whitespace parser
  -> Parsec (IndentOpt a b) -- ^ @ref@, how to parse a reference token.
  -> Parsec b -- ^ 
indentBlock ws indentOpt = do
  refLvl <- ws *> indentLevel
  refToken <- indentOpt
  case refToken of
    IndentNone x -> pure x
    IndentMany indentLvl p -> Internal.indentMany (setLvl refLvl indentLvl) ws p
    IndentSome indentLvl p -> Internal.indentSome (setLvl refLvl indentLvl) ws p

  where
    setLvl :: a -> Maybe a -> Maybe a
    setLvl refLvl = Just . fromMaybe refLvl




{-| This behaves like the megaparsec version; this documentation is adapted from the original 
[megaparsec](https://hackage.haskell.org/package/megaparsec-9.7.0/docs/src/Text.Megaparsec.Char.Lexer.html#lineFold).

Creates a parser that supports line-folding. 

@'lineFoldMegaparsec' ws p@ takes a whitespace parser @ws@, and a parser @p@ parameterised by a whitespace parser 
(i.e. it is a function expecting a whitespace parser, returning a parser).

The whitespace parser @ws@ consumes whitespace between each component of the line fold, 
so it /must/ consume newlines.
The parser @p@ receives a customised whitespace parser that checks indentation automatically,
which @p@ may then use to consume the line fold.

@since 0.4.0.0

-}
lineFoldMegaparsec
  :: Parsec () -- ^ @ws@, the whitespace parser. Must consume newlines.
  -> (Parsec () -> Parsec a) -- ^ @p@, a parser parameterised by a whitespace consumer.
  -> Parsec a
lineFoldMegaparsec ws p = do
  refLvl <- ws *> indentLevel
  let ws' = void (indentGuard ws GT refLvl)
  p ws'


