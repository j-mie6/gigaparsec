{-# LANGUAGE Safe #-}
{-|
Module      : Text.Gigaparsec.Combinator
Description : This module contains a huge number of pre-made combinators that are
              very useful for a variety of purposes.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains a huge number of pre-made combinators that are very useful for a variety of purposes.

In particular, it contains combinators for: performing a parser iteratively, collecting all the results;
querying whether or not any input is left; optionally performing parsers; parsing delimited constructions;
handling multiple possible alternatives or parsers to sequence; handling more complex conditional execution;
and more.

@since 0.1.0.0
-}
module Text.Gigaparsec.Combinator (
  -- * Iterative Combinators
  -- | These combinators all execute a given parser an unbounded number of times, until either it fails, or another
  -- parser succeeds, depending on the combinator. Depending on the combinator, all of the results produced by the
  -- repeated execution of the parser may be returned in a @[]@. These are almost essential for any practical parsing
  -- task.
    manyN, skipMany, skipSome, skipManyN, count, count1, manyTill, someTill,

  -- * Optional Parsing Combinators
  -- | These combinators allow for the /possible/ parsing of some parser. If the parser succeeds, that is ok
  -- so long as it __did not consume input__. Be aware that the result of the success may be replaced with
  -- these combinators, with the exception of "option", which still preserves the result.
    option, optional, optionalAs, decide, fromMaybeS,

  -- * Separated Values Combinators
  -- | These combinators are concerned with delimited parsing, where one parser is repeated but delimited by another one.
  -- In each of these cases @p@ is the parser of interest and @sep@ is the delimeter. These combinators mainly differ
  -- in either the number of @p@s they require, or exactly where the delimeters are allowed (only between, always
  -- trailing, or either). In all cases, they return the list of results generated by the repeated parses of @p@.
    sepBy, sepBy1, sepEndBy, sepEndBy1, endBy, endBy1,

  -- * Multiple Branching/Sequencing Combinators
  -- | These combinators allow for testing or sequencing a large number of parsers in one go.
    choice, sequence, traverse, skip,

  -- * Range Combinators
  -- | These combinators allow for the parsing of a specific parser either a specific number of times, or between a certain
  -- amount of times.
    exactly, range, range_, countRange,

  -- * Selective Combinators
  -- | These combinators allow for the conditional extraction of a result, or the execution of a parser
  -- based on another. They are derived from "branch".
    ifS, whenS, guardS, whileS,
  ) where

import Text.Gigaparsec (Parsec, many, some, (<|>), ($>), (<:>), select,
                        branch, empty, unit, manyl, somel, notFollowedBy, liftA2)
import Data.Foldable (asum, sequenceA_)

{-|
This combinator tries to parse each of the parsers @ps@ in order, until one of them succeeds.

Finds the first parser in @ps@ which succeeds, returning its result. If Nothing of the parsers
succeed, then this combinator fails. If a parser fails having consumed input, this combinator
fails __immediately__.

==== __Examples__
>>> let p = choice [string "abc", string "ab", string "bc", string "d"]
>>> parse p "abc"
Success "abc"
>>> parse p "ab"
Failure ..
>>> parse p "bc"
Success "bc"
>>> parse p "x"
Failure ..

@since 0.1.0.0
-}
choice :: [Parsec a] -- ^ the parsers, @ps@ to try, in order.
       -> Parsec a   -- ^ a parser that tries to parse one of @ps@.
choice = asum

{-|
This combinator will parse each of @ps@ in order, discarding the results.

Given the parsers @ps@, consisting of @p1@ through @pn@, parses
each in order. If they all succeed, this combinator succeeds. If any of
the parsers fail, then the whole combinator fails.

==== __Examples__
>>> let p = skip [char'a', item, char 'c']
>>> parse p "abc"
Success ()
>>> parse p "ab"
Failure ..

@since 0.1.0.0
-}
skip :: [Parsec a] -- ^ parsers @ps@ to be sequenced.
     -> Parsec ()  -- ^ a parser that parses each of @ps@, returning @()@.
skip = sequenceA_

{-|
This combinator tries to parse @p@, wrapping its result in a @Just@ if it succeeds, or returns @Nothing@ if it fails.

Tries to parse @p@. If @p@ succeeded, producing @x@, then @Just x@ is returned. Otherwise, if @p@ failed
__without consuming input__, then @Nothing@ is returned instead.

==== __Examples__
>>> let p = option (string "abc")
>>> parse p ""
Success Nothing
>>> parse p "abc"
Success (Just "abc")
>>> parse p "ab"
Failure ..

@since 0.1.0.0
-}
option :: Parsec a         -- ^ the parser @p@ to try to parse
       -> Parsec (Maybe a)
option p = Just <$> p <|> pure Nothing

{-|
This combinator will parse @p@ if possible, otherwise will do nothing.

Tries to parse @p@. If @p@ succeeds, or fails __without consuming input__ then this combinator is successful. Otherwise, if @p@ failed
having consumed input, this combinator fails.

==== __Examples__
>>> let p = optional (string "abc")
>>> parse p ""
Success ()
>>> parse p "abc"
Success ()
>>> parse p "ab"
Failure ..

@since 0.1.0.0
-}
optional :: Parsec a -- ^ the parser @p@ to try to parse.
         -> Parsec ()
optional = optionalAs ()

{-|
This combinator will parse @p@ if possible, otherwise will do nothing.

Tries to parse @p@. If @p@ succeeds, or fails __without consuming input__ then this combinator is successful and returns @x@. Otherwise,
if @p@ failed having consumed input, this combinator fails.

==== __Examples__
>>> let p = optionalAs 7 (string "abc")
>>> parse p ""
Success 7
>>> parse p "abc"
Success 7
>>> parse p "ab"
Failure ..

@since 0.1.0.0
-}
optionalAs :: b        -- ^ the value @x@ to return regardless of how @p@ performs.
           -> Parsec a -- ^ the parser @p@ to try to parse.
           -> Parsec b -- ^ a parser that tries to parse @p@, returning @x@ regardless of success or failure.
optionalAs x p = p $> x <|> pure x

-- TODO: collect
{-|
This combinator can eliminate an @Maybe@ from the result of the parser @p@.

First parse @p@, if it succeeds returning @Just x@, then return @x@. However,
if @p@ fails, or returned @Nothing@, then this combinator fails.

==== __Examples__
@decide (option p) = p@
-}
decide :: Parsec (Maybe a) -- ^ the parser @p@ to parse and extract the result from.
       -> Parsec a         -- ^ a parser that tries to extract the result from @p@.
decide p = p >>= maybe empty pure

-- this is decide overload
{-|
This combinator parses @q@ depending only if @p@ returns a @Nothing@.

First parses @p@. If @p@ returned @Just x@, then @x@ is returned.
Otherwise, if @p@ returned @Nothing@ then @q@ is parsed, producing @y@,
and @y@ is returned. If @p@ or @q@ fails, the combinator fails.

==== __Examples__
fromMaybe q (option p) = p <|> q

@since 0.1.0.0
-}
fromMaybeS :: Parsec a         -- ^ a parser to execute when @p@ returns @Nothing@, to provide a value of type @a@.
           -> Parsec (Maybe a) -- ^ the first parser @p@, which returns an @Maybe@ to eliminate.
           -> Parsec a         -- ^ a parser that either just parses @p@ or both @p@ and @q@ in order to return an @a@.
fromMaybeS q p = select (maybe (Left ()) Right <$> p) (const <$> q)

{-|
This combinator repeatedly parses a given parser __@n@__ or more times, collecting the results into a list.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will return all of the results, @x1@ through @xm@ (with @m >= n@), in a list: @[x1, .., xm]@.
If @p@ was not successful at least @n@ times, this combinator fails.

==== __Examples__
>>> let p = manyN 2 (string "ab")
>>> parse p ""
Failure ..
>>> parse p "ab"
Failure ..
>>> parse p "abababab"
Success ["ab", "ab", "ab", "ab"]
>>> parse p "aba"
Failure ..

==== Notes
* @many p == many 0 p@ and @some p == many 1 p@.

@since 0.1.0.0
-}
manyN :: Int        -- ^ the minimum number of @p@s required, @n@.
      -> Parsec a   -- ^ the parser @p@ to execute multiple times.
      -> Parsec [a] -- ^ a parser that parses @p@ until it fails, returning the list of all the successful results.
manyN 0 p = many p
manyN 1 p = some p
manyN n p = p <:> manyN (n - 1) p

{-|
This combinator repeatedly parses a given parser __zero__ or more times, ignoring the results.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will succeed.

==== __Examples__
>>> let p = skipMany (string "ab")
>>> parse p ""
Success ()
>>> parse p "ab"
Success ()
>>> parse p "abababab"
Success ()
>>> parse p "aba"
Failure ..

@since 0.1.0.0
-}
skipMany :: Parsec a  -- ^ the parser @p@ to execute multiple times.
         -> Parsec () -- ^ a parser that parses @p@ until it fails, returning unit.
skipMany p = let go = p *> go <|> unit in go

{-|
This combinator repeatedly parses a given parser __one__ or more times, ignoring the results.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will succeed. The parser @p@ must succeed at least once.

==== __Examples__
>>> let p = skipSome (string "ab")
>>> parse p ""
Failure ..
>>> parse p "ab"
Success ()
>>> parse p "abababab"
Success ()
>>> parse p "aba"
Failure ..

@since 0.1.0.0
-}
skipSome :: Parsec a  -- ^ @p@, the parser to execute multiple times.
         -> Parsec () -- ^ a parser that parses @p@ until it fails, returning unit.
skipSome p = p *> skipMany p

{-|
This combinator repeatedly parses a given parser __@n@__ or more times, ignoring the results.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will succeed. The parser @p@ must succeed at least @n@ times.

==== __Examples__
>>> let p = skipManyN 2 (string "ab")
>>> parse p ""
Failure ..
>>> parse p "ab"
Failure ..
>>> parse p "abababab"
Success ()
>>> parse p "aba"
Failure ..

@since 0.1.0.0
-}
skipManyN :: Int       -- ^ @n@, the minimum number of times to execute.
          -> Parsec a  -- ^ @p@, the parser to execute multiple times.
          -> Parsec () -- ^ a parser that parses @p@ until it fails, returning unit.
skipManyN 0 p = skipMany p
skipManyN 1 p = skipSome p
skipManyN n p = p *> skipManyN (n - 1) p

{-|
This combinator repeatedly parses a given parser __zero__ or more times, returning how many times it succeeded.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will succeed. The number of times @p@ succeeded is returned as the result.

==== __Examples__
>>> let p = count (string "ab")
>>> parse p ""
Success 0
>>> parse p "ab"
Success 1
>>> parse p "abababab"
Success 4
>>> parse p "aba"
Failure ..

@since 0.1.0.0
-}
count :: Parsec a   -- ^ @p@, the parser to execute multiple times.
      -> Parsec Int -- ^ the number of times @p@ successfully parses
count = manyl (flip (const (+ 1))) 0

{-|
This combinator repeatedly parses a given parser __one__ or more times, returning how many times it succeeded.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. Otherwise when @p@ fails __without consuming input__, this combinator
will succeed. The parser @p@ must succeed at least once. The number of times @p@ succeeded is returned as the result.

==== __Examples__
>>> let p = count1 (string "ab")
>>> parse p ""
Failure ..
>>> parse p "ab"
Success 1
>>> parse p "abababab"
Success 4
>>> parse p "aba"
Failure ..

@since 0.1.0.0
-}
count1 :: Parsec a   -- ^ @p@, the parser to execute multiple times.
       -> Parsec Int -- ^ the number of times @p@ successfully parses
count1 = somel (flip (const (+ 1))) 0

{-|
This combinator parses __zero__ or more occurrences of @p@, separated by @sep@.

Behaves just like @sepBy1@, except does not require an initial @p@, returning the empty list instead.

==== __Examples__
>>> ...
>>> let args = sepBy int (string ", ")
>>> parse args "7, 3, 2"
Success [7, 3, 2]
>>> parse args ""
Success []
>>> parse args "1"
Success [1]
>>> parse args "1, 2, "
Failure ..

@since 0.1.0.0
-}
sepBy :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
      -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
      -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
sepBy p sep = sepBy1 p sep <|> pure []

{-|
This combinator parses __one__ or more occurrences of @p@, separated by @sep@.

First parses a @p@. Then parses @sep@ followed by @p@ until there are no more @sep@s.
The results of the @p@'s, @x1@ through @xn@, are returned as @[x1, .., xn]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails. Requires at least
one @p@ to have been parsed.

==== __Examples__
>>> ...
>>> let args = sepBy1 int (string ", ")
>>> parse args "7, 3, 2"
Success [7, 3, 2]
>>> parse args ""
Failure ..
>>> parse args "1"
Success [1]
>>> parse args "1, 2, "
Failure ..

@since 0.1.0.0
-}
sepBy1 :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
       -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
       -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
sepBy1 p sep = p <:> many (sep *> p)

{-|
This combinator parses __zero__ or more occurrences of @p@, separated and optionally ended by @sep@.

Behaves just like @sepEndBy1@, except does not require an initial @p@, returning the empty list instead.

==== __Examples__
>>> ...
>>> let args = sepEndBy int (string ";\n")
>>> parse args "7;\n3;\n2"
Success [7, 3, 2]
>>> parse args ""
Success Nil
>>> parse args "1"
Success [1]
>>> parse args "1;\n2;\n"
Success [1, 2]

@since 0.1.0.0
-}
sepEndBy :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
         -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
         -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
sepEndBy p sep = sepEndBy1 p sep <|> pure []

{-|
This combinator parses __one__ or more occurrences of @p@, separated and optionally ended by @sep@.

First parses a @p@. Then parses @sep@ followed by @p@ until there are no more: if a final @sep@ exists, this is parsed.
The results of the @p@'s, @x1@ through @xn@, are returned as @[x1, .., xn]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails. Requires at least
one @p@ to have been parsed.

==== __Examples__
>>> ...
>>> let args = sepEndBy1 int (string ";\n")
>>> parse args "7;\n3;\n2"
Success [7, 3, 2]
>>> parse args ""
Failure ..
>>> parse args "1"
Success [1]
>>> parse args "1;\n2;\n"
Success [1, 2]

@since 0.1.0.0
-}
sepEndBy1 :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
          -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
          -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
sepEndBy1 p sep = let seb1 = p <:> (sep *> (seb1 <|> pure []) <|> pure []) in seb1

{-|
This combinator parses __zero__ or more occurrences of @p@, separated and ended by @sep@.

Behaves just like @endBy1@, except does not require an initial @p@ and @sep@, returning the empty list instead.

==== __Examples__
>>> ...
>>> let args = endBy int (string ";\n")
>>> parse args "7;\n3;\n2"
Failure ..
>>> parse args ""
Success Nil
>>> parse args "1;\n"
Success [1]
>>> parse args "1;\n2;\n"
Success [1, 2]

@since 0.1.0.0
-}
endBy :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
      -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
      -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
endBy p sep = endBy1 p sep <|> pure []

{-|
This combinator parses __one__ or more occurrences of @p@, separated and ended by @sep@.

Parses @p@ followed by @sep@ one or more times.
The results of the @p@'s, @x1@ through @xn@, are returned as @[x1, .., xn]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails.

==== __Examples__
>>> ...
>>> let args = endBy1 int (string ";\n")
>>> parse args "7;\n3;\n2"
Failure ..
>>> parse args ""
Failure ..
>>> parse args "1;\n"
Success [1]
>>> parse args "1;\n2;\n"
Success [1, 2]

@since 0.1.0.0
-}
endBy1 :: Parsec a   -- ^ @p@, the parser whose results are collected into a list.
       -> Parsec sep -- ^ @sep@, the delimiter that must be parsed between every @p@.
       -> Parsec [a] -- ^ a parser that parses @p@ delimited by @sep@, returning the list of @p@'s results.
endBy1 p sep = some (p <* sep)

{-|
This combinator repeatedly parses a given parser __zero__ or more times, until the @end@ parser succeeds, collecting the results into a list.

First tries to parse @end@, if it fails __without consuming input__, then parses @p@, which must succeed. This repeats until @end@ succeeds.
When @end@ does succeed, this combinator will return all of the results generated by @p@, @x1@ through @xn@ (with @n >= 0@), in a
list: @[x1, .., xn]@. If @end@ could be parsed immediately, the empty list is returned.

==== __Examples__
This can be useful for scanning comments:
>>> let comment = string "--" *> manyUntil item endOfLine
>>> parse p "--hello world"
Failure ..
>>> parse p "--hello world\n"
Success ['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']
>>> parse p "--\n"
Success Nil

@since 0.1.0.0
-}
manyTill :: Parsec a   -- ^ @p@, the parser to execute multiple times.
         -> Parsec end -- ^ @end@, the parser that stops the parsing of @p@.
         -> Parsec [a] -- ^ a parser that parses @p@ until @end@ succeeds, returning the list of all the successful results.
manyTill p end = let go = end $> [] <|> p <:> go in go

{-|
This combinator repeatedly parses a given parser __one__ or more times, until the @end@ parser succeeds, collecting the results into a list.

First ensures that trying to parse @end@ fails, then tries to parse @p@. If it succeed then it will repeatedly: try to parse @end@, if it fails
__without consuming input__, then parses @p@, which must succeed. When @end@ does succeed, this combinator will return all of the results
generated by @p@, @x1@ through @xn@ (with @n >= 1@), in a list: @[x1, .., xn]@. The parser @p@ must succeed at least once
before @end@ succeeds.

==== __Examples__
This can be useful for scanning comments:
>>> let comment = string "--" *> someUntil item endOfLine
>>> parse p "--hello world"
Failure ..
>>> parse p "--hello world\n"
Success ['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']
>>> parse p "--\n"
Failure ..
>>> parse p "--a\n"
Success ['a']

@since 0.1.0.0
-}
someTill :: Parsec a   -- ^ @p@, the parser to execute multiple times.
         -> Parsec end -- ^ @end@, the parser that stops the parsing of @p@.
         -> Parsec [a] -- ^ a parser that parses @p@ until @end@ succeeds, returning the list of all the successful results.
someTill p end = notFollowedBy end *> (p <:> manyTill p end)

-- this is ifP
{-|
This combinator parses one of @thenP@ or @elseP@ depending on the result of parsing @condP@.

This is a lifted @if@-statement. First, parse @condP@: if it is successful and returns
@true@, then parse @thenP@; else, if it returned @false@, parse @elseP@; or, if @condP@ failed
then fail. If either of @thenP@ or @elseP@ fail, then this combinator also fails.

Most useful in conjunction with /Registers/, as this allows for decisions to be made
based on state.

==== __Examples__
>>> ifP (pure true) p _ == p
>>> ifP (pure false) _ p == p

@since 0.1.0.0
-}
ifS :: Parsec Bool -- ^ @condP@, the parser that yields the condition value.
    -> Parsec a    -- ^ @thenP@, the parser to execute if the condition is @true@.
    -> Parsec a    -- ^ @elseP@, the parser to execute if the condition is @false.
    -> Parsec a    -- ^ a parser that conditionally parses @thenP@ or @elseP@ after @condP@.
ifS cond t e = branch (bool <$> cond) (const <$> e) (const <$> t)
  where bool True = Right ()
        bool False = Left ()

-- this is when
{-|
This combinator conditionally parses @thenP@ depending on the result of parsing @condP@.

This is a lifted @if@-statement. First, parse @condP@: if it is successful and returns
@true@, then parse @thenP@; else, if it returned @false@ do nothing; or, if @condP@ failed
then fail. If @thenP@ fails, then this combinator also fails.

Most useful in conjunction with /Registers/, as this allows for decisions to be made
based on state.

==== __Examples__
>>> when (pure true) p == p
>>> when (pure false) _ == unit

@since 0.1.0.0
-}
whenS :: Parsec Bool -- ^ @condP@, the parser that yields the condition value.
      -> Parsec ()   -- ^ @thenP@, the parser to execute if the condition is @true@.
      -> Parsec ()   -- ^ a parser that conditionally parses @thenP@ after @condP@.
whenS cond p = ifS cond p unit

-- this is guard
{-|
This combinator verfies that the given parser returns @true@, or else fails.

First, parse @p@; if it succeeds then, so long at returns @true@, this @guard p@ succeeds. Otherwise,
if @p@ either fails, or returns @false@, @guard p@ will fail.

==== __Examples__
>>> guard (pure true) == unit
>>> guard (pure false) == empty
>>> when (not <$> p) empty == guard p

@since 0.1.0.0
-}
guardS :: Parsec Bool -- ^ @p@, the parser that yields the condition value.
       -> Parsec ()
guardS cond = ifS cond unit empty

-- this is whileP
{-|
This combinator repeatedly parses @p@ so long as it returns @true@.

This is a lifted @while@-loop. First, parse @p@: if it is successful and
returns @true@, then repeat; else if it returned @false@ stop; or, if it
failed then this combinator fails.

Most useful in conjunction with /Registers/, as this allows for decisions to be made
based on state. In particular, this can be used to define the @forP@ combinator.

@since 0.1.0.0
-}
whileS :: Parsec Bool -- ^ @p@, the parser to repeatedly parse.
       -> Parsec ()   -- ^ a parser that continues to parse @p@ until it returns @false@.
whileS c = let go = whenS c go in go

{-|
This combinator parses exactly @n@ occurrences of @p@, returning these @n@ results in a list.

Parses @p@ repeatedly up to @n@ times. If @p@ fails before @n@ is reached, then this combinator
fails. It is not required for @p@ to fail after the @n@th parse. The results produced by
@p@, @x1@ through @xn@, are returned as @[x1, .., xn]@.

==== __Examples__
>>> let p = exactly 3 item
>>> parse p "ab"
Failure ..
>>> parse p "abc"
Success ['a', 'b', 'c']
>>> parse p "abcd"
Success ['a', 'b', 'c']

@since 0.1.0.0
-}
exactly :: Int        -- ^ @n@, the number of times to repeat @p@.
        -> Parsec a   -- ^ @p@, the parser to repeat.
        -> Parsec [a] -- ^ a parser that parses @p@ exactly @n@ times, returning a list of the results.
exactly n = range n n

{-|
This combinator parses between @min@ and @max@ occurrences of @p@, returning these @n@ results in a list.

Parses @p@ repeatedly a minimum of @min@ times and up to @max@ times both inclusive. If @p@ fails before
@min@ is reached, then this combinator fails. It is not required for @p@ to fail after the @max@^th^ parse.
The results produced by @p@, @xmin@ through @xmax@, are returned as @[xmin, .., xmax]@.

==== __Examples__
>>> let p = range 3 5 item
>>> parse p "ab"
Failure ..
>>> parse p "abc"
Success ['a', 'b', 'c']
>>> parse p "abcd"
Success ['a', 'b', 'c', 'd']
>>> parse p "abcde"
Success ['a', 'b', 'c', 'd', 'e']
>>> parse p "abcdef"
Success ['a', 'b', 'c', 'd', 'e']

@since 0.1.0.0
-}
range :: Int        -- ^ @min@, the minimum number of times to repeat @p@, inclusive.
      -> Int        -- ^ @max@, the maximum number of times to repeat @p@, inclusive.
      -> Parsec a   -- ^ @p@, the parser to repeat.
      -> Parsec [a] -- ^ the results of the successful parses of @p@.
range mn mx p
  | mn < 0 || mx < mn = pure []
  | otherwise = go mn mx
  where
    go 0 0 = pure []
    go 0 n = p <:> go 0 (n - 1) <|> pure []
    go m n = p <:> go (m - 1) (n - 1)

{-|
This combinator parses between @min@ and @max@ occurrences of @p@ but ignoring the results.

Parses @p@ repeatedly a minimum of @min@ times and up to @max@ times both inclusive. If @p@ fails before
@min@ is reached, then this combinator fails. It is not required for @p@ to fail after the @max@th parse.
The results are discarded and @()@ is returned instead.

==== __Examples__
>>> let p = range_ 3 5 item
>>> parse p "ab"
Failure ..
>>> parse p "abc"
Success ()
>>> parse p "abcd"
Success ()
>>> parse p "abcde"
Success ()
>>> parse p "abcdef"
Success ()

@since 0.1.0.0
-}
range_ :: Int       -- ^ @min@, the minimum number of times to repeat @p@, inclusive.
       -> Int       -- ^ @max@, the maximum number of times to repeat @p@, inclusive.
       -> Parsec a  -- ^ @p@, the parser to repeat.
       -> Parsec ()
range_ mn mx p
  | mn < 0 || mx < mn = unit
  | otherwise = go mn mx
  where
    go 0 0 = unit
    go 0 n = optional (p *> go 0 (n - 1))
    go m n = p *> go (m - 1) (n - 1)

-- this is count overloading
{-|
This combinator parses between @min@ and @max@ occurrences of @p@, returning the number of successes.

Parses @p@ repeatedly a minimum of @min@ times and up to @max@ times both inclusive. If @p@ fails before
@min@ is reached, then this combinator fails. It is not required for @p@ to fail after the @max@th parse.
The results are discarded and the number of successful parses of @p@, @n@, is returned instead, such that
@min <= n <= max@.

==== __Examples__
>>> let p = count 3 5 item
>>> parse p "ab"
Failure ..
>>> parse p "abc"
Success 3
>>> parse p "abcd"
Success 4
>>> parse p "abcde"
Success 5
>>> parse p "abcdef"
Success 5

@since 0.1.0.0
-}
countRange :: Int        -- ^ @min@, the minimum number of times to repeat @p@, inclusive.
           -> Int        -- ^ @max@, the maximum number of times to repeat @p@, inclusive.
           -> Parsec a   -- ^ @p@, the parser to repeat.
           -> Parsec Int -- ^ the number of times @p@ parsed successfully.
countRange mn mx p
  | mn < 0 || mx < mn = pure 0
  | otherwise = go mn mx
  where
    go 0 0 = pure 0
    go 0 n = liftA2 (const (+ 1)) p (go 0 (n - 1)) <|> pure 0
    go m n = liftA2 (const (+ 1)) p (go (m - 1) (n - 1))
