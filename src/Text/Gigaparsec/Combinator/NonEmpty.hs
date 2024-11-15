{-# LANGUAGE Safe #-}
{-|
Module      : Text.Gigaparsec.Combinator.NonEmpty
Description : This module contains variants of combinators that return non-empty lists of results, modifying the result type to 'NonEmpty'.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains variants of combinators that return non-empty lists of results, modifying the result type to 'NonEmpty'.
These allow for stronger guarantees of parsed results to be baked into their types.

@since 0.3.0.0
-}
module Text.Gigaparsec.Combinator.NonEmpty (some, someTill, sepBy1, sepEndBy1, endBy1) where

import Text.Gigaparsec (Parsec, notFollowedBy)
import Text.Gigaparsec.Combinator qualified as Combinator (manyTill, sepEndBy1)

import Control.Applicative (liftA2, many)
import Data.List.NonEmpty as NonEmpty (NonEmpty((:|)), fromList)

infixl 4 <:|>
(<:|>) :: Parsec a -> Parsec [a] -> Parsec (NonEmpty a)
(<:|>) = liftA2 (:|)

{-
infixl 4 <<|>
(<<|>) :: Parsec a -> Parsec (NonEmpty a) -> Parsec (NonEmpty a)
(<<|>) = liftA2 (<|)
-}

{-| 
This combinator repeatedly parses a given parser __one__ or more times, collecting the results into a non-empty list.

Parses a given parser, @p@, repeatedly until it fails. If @p@ failed having consumed input,
this combinator fails. 
Otherwise, when @p@ fails __without consuming input__, this combinator
will return all of the results, @x₁@ through @xₙ@ (with @n ≥ 1@), in a non-empty list: @x₁ :| [x₂, .., xₙ]@.

Requires at least one @p@ to have been parsed.

@since 0.3.0.0
-}
some  :: Parsec a            -- ^ the parser @p@ to execute multiple times
      -> Parsec (NonEmpty a) -- ^ a parser that parses @p@ until it fails, returning a non-empty list of the successful results.
some p = p <:|> many p

{-|
This combinator repeatedly parses a given parser __one__ or more times, until the @end@ parser succeeds, collecting the results into a non-empty list.

First ensures that trying to parse @end@ fails, then tries to parse @p@. If it succeeds then it will repeatedly: try to parse @end@, if it fails
__without consuming input__, then parses @p@, which must succeed. 
When @end@ does succeed, this combinator will return all of the results
generated by @p@, @x₁@ through @xₙ@ (with @n ≥ 1@), in a non-empty list: @x₁ :| [x₂, .., xₙ]@.

The parser @p@ must succeed at least once before @end@ succeeds.

@since 0.3.0.0
-}
someTill  :: Parsec a            -- ^ @p@, the parser to execute multiple times, at least once.
          -> Parsec end          -- ^ @end@, the parser that stops the parsing of @p@.
          -> Parsec (NonEmpty a) -- ^ a parser that parses @p@ until @end@ succeeds, returning the non-empty list of all the successful results.
someTill p end = notFollowedBy end *> (p <:|> Combinator.manyTill p end)

{-|
This combinator parses __one__ or more occurrences of @p@, separated by @sep@.

First parses a @p@. Then parses @sep@ followed by @p@ until there are no more @sep@s.
The results of the @p@'s, @x₁@ through @xₙ@, are returned as @x₁ :| [x₂, .., xₙ]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails.

Requires at least one @p@ to have been parsed.

@since 0.3.0.0
-}
sepBy1  :: Parsec a            -- ^ @p@, the parser whose results are collected into a non-empty list.
        -> Parsec sep          -- ^ @sep@, the delimiter that must be parsed between every @p@.
        -> Parsec (NonEmpty a) -- ^ a parser that parses @p@ delimited by @sep@, returning the non-empty list of @p@'s results.
sepBy1 p sep = p <:|> many (sep *> p)

{-|
This combinator parses __one__ or more occurrences of @p@, separated and ended by @sep@.

Parses @p@ followed by @sep@ one or more times.
The results of the @p@'s, @x₁@ through @xₙ@, are returned as @x₁ :| [x₂, .., xₙ]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails.

Requires at least one @p@ to have been parsed.

@since 0.3.0.0
-}
endBy1  :: Parsec a            -- ^ @p@, the parser whose results are collected into a non-empty list.
        -> Parsec sep          -- ^ @sep@, the delimiter that must be parsed between every @p@.
        -> Parsec (NonEmpty a) -- ^ a parser that parses @p@ delimited by @sep@, returning the non-empty list of @p@'s results.
endBy1 p sep = some (p <* sep)

{-|
This combinator parses __one__ or more occurrences of @p@, separated and optionally ended by @sep@,
collecting the results in a non-empty list.

First parses a @p@. Then parses @sep@ followed by @p@ until there are no more: if a final @sep@ exists, this is parsed.
The results of the @p@'s, @x₁@ through @xₙ@, are returned as @x₁ :| [x₂, .., xₙ]@.
If @p@ or @sep@ fails having consumed input, the whole parser fails. 

Requires at least one @p@ to have been parsed.

@since 0.3.0.0
-}
sepEndBy1 :: Parsec a            -- ^ @p@, the parser whose results are collected into a list.
          -> Parsec sep          -- ^ @sep@, the delimiter that must be parsed between every @p@.
          -> Parsec (NonEmpty a) -- ^ a parser that parses @p@ delimited by @sep@, returning the non-empty list of @p@'s results.
sepEndBy1 p sep = NonEmpty.fromList <$> Combinator.sepEndBy1 p sep
