{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-|
Module      : Text.Gigaparsec.Char
Description : Contains the combinators needed to read characters and strings, as well as combinators
              to match specific sub-sets of characters.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains many parsers to do with reading one or more characters. Almost every parser
will need something from this module.

In particular, this module contains: combinators that can read specific characters; combinators that
represent character classes and their negations; combinators for reading specific strings; as well
as a selection of pre-made parsers to parse specific kinds of character, like digits and letters.

@since 0.1.0.0
-}
module Text.Gigaparsec.Char (
  -- * Core Combinators
  -- | These are the most primitive combinators for consuming input capable of any input reading tasks.
    char, item, satisfy, satisfyMap,
  -- * Character Class Combinators
  -- | These combinators allow for working with /character classes/. This means that a set, or range,
  -- of characters can be specified, and the combinator will return a parser that matches one of
  -- those characters (or conversely, any character that is /not/ in that set). The parsed character
  -- is always returned.
    oneOf, noneOf,
  -- * String Combinators
  -- | These combinators allow for working with, or building, strings. This means that they can parse
  -- specific strings, specific sets of strings, or can read characters repeatedly to generate
  -- strings. They are united in all returning `String` as their result.
    string, {-stringOfMany, stringOfSome,-} strings, trie,
  -- * Specific Character Parsers
  -- | These parsers are special cases of `satisfy` or `char`. They are worth
  -- using, as they are given special error labelling, producing nicer error messages than their
  -- primitive counterparts.
    bit, crlf, digit, endOfLine, hexDigit, letter, letterOrDigit, lower, newline, octDigit, space,
    tab, upper, whitespace,
  -- * Whitespace Skipping Parsers
  -- | These parsers are designed to skip chunks of whitespace, for very rudimentary lexing tasks. It
  -- is probably better to use the functionality of "Gigaparsec.Token".
    spaces, whitespaces,
  ) where

import Text.Gigaparsec (Parsec, atomic, empty, (<|>), many, void)
import Text.Gigaparsec.Errors.Combinator ((<?>))
-- We want to use this to make the docs point to the right definition for users.
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec))
import Text.Gigaparsec.Internal.Require (require)

import Data.Char (ord)
import Data.Char qualified as Char
import Data.List.NonEmpty as NonEmpty (NonEmpty((:|)), groupWith, sortBy)
import Data.Maybe (isJust, fromJust)
import Data.Monoid (Alt(Alt, getAlt))
import Data.Set (Set)
import Data.Set qualified as Set (member, size, findMin, findMax, mapMonotonic)
import Data.Ord (Down(Down), comparing)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map (fromSet, toAscList, member)

-------------------------------------------------
-- Primitives
-------------------------------------------------

{-|
This combinator tries to parse a single character from the input that matches the given predicate.

Attempts to read a character from the input and tests it against the predicate @pred@. If a character @c@
can be read and @pred c@ is true, then @c@ is consumed and returned. Otherwise, no input is consumed
and this combinator will fail.

>>> parse (satisfy Data.Char.isDigit) ""
Failure ..
>>> parse (satisfy Data.Char.isDigit) "7"
Success '7'
>>> parse (satisfy Data.Char.isDigit) "a5"
Failure ..

Roughly speaking:

@
char c = satisfy (== c)
@

@since 0.1.0.0
-}
satisfy :: (Char -> Bool) -- ^ the predicate, @pred@, to test the next character against, should one exist.
        -> Parsec Char    -- ^ a parser that tries to read a single character @c@, such that @pred c@ is true, or fails.
satisfy _ = Internal.Parsec undefined --TODO:

-- Needs to be primitive for the raw expected item down the line
{-|
This combinator tries to parse a single specific character @c@ from the input.

Attempts to read the given character @c@ from the input stream at the current position. If this
character can be found, it is consumed and returned. Otherwise, no input is consumed and this
combinator will fail.

>>> parse (char 'a') ""
Failure ..
>>> parse (char 'a') "a"
Success 'a'
>>> parse (char 'a') "ba"
Failure ..

@since 0.1.0.0
-}
char :: Char        -- ^ the character to parse, @c@.
     -> Parsec Char -- ^ a parser that tries to read a single @c@, or fails.
char c = satisfy (== c)

-- Needs to be primitive for the raw expected item and wide caret down the line
{-|
TODO:

@since 0.1.0.0
-}
string :: String -> Parsec String
string s = require (not (null s)) "cannot pass empty string to `string`" $
  traverse char s

-------------------------------------------------
-- Composite Combinators
-------------------------------------------------

-- Could be optimised to remove the partiality
{-|
TODO:

@since 0.1.0.0
-}
satisfyMap :: (Char -> Maybe a) -> Parsec a
satisfyMap f = fromJust . f <$> satisfy (isJust . f)

-- Use RangeSet internally?
-- the labelling here should be consistent with raw characters, not sure how
-- other than hard-coding it like Scala parsley?
{-|
TODO:

@since 0.1.0.0
-}
oneOf :: Set Char -> Parsec Char
oneOf cs
  | sz == 0                 = empty
  | sz == 1                 = char c1
  -- if the smallest and largest characters are as far apart
  -- as the size of the set, it must be contiguous
  | sz == (ord c2 - ord c1) = satisfy (\c -> c1 <= c && c <= c2) <?> Set.mapMonotonic show cs
  | otherwise               = satisfy (`Set.member` cs) <?> [rangeLabel]
  where !sz = Set.size cs
        -- must be left lazy until sz known not to be 0
        c1 = Set.findMin cs
        c2 = Set.findMax cs
        --FIXME: control character safe show (and for the map above!)
        rangeLabel = "one of " ++ show c1 ++ " to " ++ show c2

{-|
TODO:

@since 0.1.0.0
-}
noneOf :: Set Char -> Parsec Char
noneOf cs
  | sz == 0                 = item
  | sz == 1                 = satisfy (/= c1) <?> ["anything except " ++ show c1]
  | sz == (ord c2 - ord c1) = satisfy (\c -> c < c1 || c2 < c) <?> [rangeLabel]
  | otherwise               = satisfy (not . (`Set.member` cs))
  where !sz = Set.size cs
        -- must be left lazy until sz known not to be 0
        c1 = Set.findMin cs
        c2 = Set.findMax cs
        --FIXME: control character safe show
        rangeLabel = "anything outside of " ++ show c1 ++ " to " ++ show c2

-- TODO: these four are tricky, because can't overload like in Scala
-- Megaparsec uses takeWhileP and takeWhile1P, but these don't really fit our
-- naming conventions...
-- stringOfMany :: Parsec Char -> Parsec String
-- stringOfSome :: Parsec Char -> Parsec String
-- stringOfMany :: (Char -> Bool) -> Parsec String
-- stringOfSome :: (Char -> Bool) -> Parsec String

{-|
TODO:

@since 0.1.0.0
-}
strings :: Set String -> Parsec String
strings = trie . Map.fromSet pure

-- Departure from original naming, but no overloading, so oh well
{-|
TODO:

@since 0.1.0.0
-}
trie :: Map String (Parsec a) -> Parsec a
trie strs = require (not (Map.member "" strs)) "cannot pass empty string to `strings` or `trie`" $
  getAlt $ foldMap combineSameLeading (NonEmpty.groupWith (head . fst) (Map.toAscList strs))
  where -- When combining these parsers it is important to make sure the
        -- longest ones parse first. All but the last parser need an `atomic`.
        combineSameLeading :: NonEmpty (String, Parsec a) -> Alt Parsec a
        combineSameLeading group = foldMap (\(str, p) -> Alt $ atomic (string str) *> p) (reverse rest)
                               <|> Alt (string longest *> longP)
          where (longest, longP) :| rest = sortOnInverse (length . fst) group

        sortOnInverse :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
        sortOnInverse f =
          fmap snd . NonEmpty.sortBy (comparing (Down . fst)) . fmap (\x -> let !y = f x in (y, x))

-------------------------------------------------
-- Specific Character Parsers
-------------------------------------------------

{-|
This parser will parse __any__ single character from the input, failing if there is no input
remaining.

@since 0.1.0.0
-}
item :: Parsec Char
item = satisfy (const True) <?> ["any character"]

{-|
This parser tries to parse a space or tab character, and returns it if successful.

@since 0.1.0.0
-}
space :: Parsec Char
space = satisfy (\c -> c == ' ' || c == '\t') <?> ["space", "tab"]

{-|
This parser skips zero or more space characters using `space`.

@since 0.1.0.0
-}
spaces :: Parsec ()
spaces = void (many space) -- TODO: skipMany

{-|
This parser tries to parse a whitespace character, and returns it if successful.

TODO: what is whitespace?

@since 0.1.0.0
-}
whitespace :: Parsec Char
whitespace = satisfy Char.isSpace <?> ["whitespace"]

{-|
This parser skips zero or more space characters using `whitespace`.

@since 0.1.0.0
-}
whitespaces :: Parsec ()
whitespaces = void (many whitespace) -- TODO: skipMany

{-|
This parser tries to parse a line feed newline (@\'\\n\'@) character, and returns it if successful.

This parser will not accept a carriage return (@CR@) character or @CRLF@.

@since 0.1.0.0
-}
newline :: Parsec Char
newline = char '\n' <?> ["newline"]

{-|
This parser tries to parse a @CRLF@ newline character pair, returning @\'\\n\'@ if successful.

A @CRLF@ character is the pair of carriage return (@\'\\r\'@) and line feed (@\'\\n\'@). These
two characters will be parsed together or not at all. The parser is made atomic using `atomic`.

@since 0.1.0.0
-}
crlf :: Parsec Char
crlf = '\n' <$ atomic (string "\r\n") <?> ["crlf"]

{-|
This parser will parse either a line feed (@LF@) or a @CRLF@ newline, returning @\'\\n\'@ if successful.

@since 0.1.0.0
-}
endOfLine :: Parsec Char
endOfLine = newline <|> crlf <?> ["end of line"]

{-|
This parser tries to parse a tab (@\'\\t\'@) character, and returns it if successful.

This parser does not recognise vertical tabs, only horizontal ones.

@since 0.1.0.0
-}
tab :: Parsec Char
tab = char '\t' <?> ["tab"]

{-|
This parser tries to parse an uppercase letter, and returns it if successful.

TODO: what is an uppercase letter?

@since 0.1.0.0
-}
upper :: Parsec Char
upper = satisfy Char.isUpper <?> ["uppercase letter"]

{-|
This parser tries to parse an lowercase letter, and returns it if successful.

TODO: what is an lowercase letter?

@since 0.1.0.0
-}
lower :: Parsec Char
lower = satisfy Char.isLower <?> ["lowercase letter"]

{-|
This parser tries to parse either a letter or a digit, and returns it if successful.

A letter or digit is anything that would parse in either `letter` or `digit`.

@since 0.1.0.0
-}
letterOrDigit :: Parsec Char
letterOrDigit = satisfy Char.isAlphaNum <?> ["alpha-numeric character"]

{-|
This parser tries to parse a letter, and returns it if successful.

TODO: what is a letter?

@since 0.1.0.0
-}
letter :: Parsec Char
letter = satisfy Char.isAlpha <?> ["letter"]

{-|
This parser tries to parse a digit, and returns it if successful.

TODO: what is a digit?

@since 0.1.0.0
-}
digit :: Parsec Char
digit = satisfy Char.isDigit <?> ["digit"]

{-|
This parser tries to parse a hexadecimal digit, and returns it if successful.

A hexadecimal digit is one of (all inclusive ranges):
  1. the digits @\'0\'@ through @\'9\'@
  1. the letters @\'a\'@ through @\'f\'@
  1. the letters @\'A\'@ through @\'Z\'@

@since 0.1.0.0
-}
hexDigit :: Parsec Char
hexDigit = satisfy Char.isHexDigit <?> ["hexadecimal digit"]

{-|
This parser tries to parse an octal digit, and returns it if successful.

An octal digit is one of @\'0\'@ to @\'7\'@ (inclusive).

@since 0.1.0.0
-}
octDigit :: Parsec Char
octDigit = satisfy Char.isOctDigit <?> ["octal digit"]

{-|
This parser tries to parse a binary digit (bit) and returns it if successful.

A bit is either @\'0\'@ or @\'1\'@.

@since 0.1.0.0
-}
bit :: Parsec Char
bit = oneOf ['0', '1'] <?> ["bit"]
