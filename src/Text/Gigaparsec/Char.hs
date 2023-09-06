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
    string, stringOfMany, stringOfSome, strings, trie,
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

import Text.Gigaparsec (Parsec, atomic, empty, some, many, (<|>))
import Text.Gigaparsec.Combinator (skipMany)
import Text.Gigaparsec.Errors.Combinator ((<?>))
-- We want to use this to make the docs point to the right definition for users.
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), State(..))
import Text.Gigaparsec.Internal.Require (require)

import Data.Bits (Bits((.&.), (.|.)))
import Data.Char (ord)
import Data.Char qualified as Char
import Data.List.NonEmpty as NonEmpty (NonEmpty((:|)), groupWith, sortWith)
import Data.Maybe (isJust, fromJust)
import Data.Monoid (Alt(Alt, getAlt))
import Data.Set (Set)
import Data.Set qualified as Set (member, size, findMin, findMax, mapMonotonic)
import Data.Map (Map)
import Data.Map qualified as Map (fromSet, toAscList, member)

-------------------------------------------------
-- Primitives
-------------------------------------------------

{-|
This combinator tries to parse a single character from the input that matches the given predicate.

Attempts to read a character from the input and tests it against the predicate @pred@. If a character
@c@ can be read and @pred c@ is true, then @c@ is consumed and returned. Otherwise, no input is
consumed and this combinator will fail.

==== __Examples__
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
satisfy :: (Char -> Bool) -- ^ the predicate, @pred@, to test the next character against, should one
                          -- exist.
        -> Parsec Char    -- ^ a parser that tries to read a single character @c@, such that @pred c@
                          -- is true, or fails.
satisfy test = Internal.Parsec $ \st ok err ->
  case Internal.input st of
    (x: xs) | test x  ->
      ok x ((updatePos st x) { Internal.input = xs, Internal.consumed = True })
    _                 -> err st
  where
  updatePos st x
    | x == '\n' = st { Internal.line = Internal.line st + 1, Internal.col = 1 }
    | x == '\t' = st { Internal.col = (((Internal.col st) + 3) .&. (-4)) .|. 1 }
    | otherwise = st { Internal.col = Internal.col st + 1 }

-- Needs to be primitive for the raw expected item down the line
{-|
This combinator tries to parse a single specific character @c@ from the input.

Attempts to read the given character @c@ from the input stream at the current position. If this
character can be found, it is consumed and returned. Otherwise, no input is consumed and this
combinator will fail.

==== __Examples__
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
This combinator attempts to parse a given string from the input, and fails otherwise.

Attempts to read the given string /completely/ from the input at the current position.
If the string is present, then the parser succeeds, and the entire string is consumed
from the input. Otherwise, if the input has too few characters remaining, or not all
the characters matched, the parser fails. On failure, __all__ the characters that were
matched are consumed from the input.

==== __Examples__
>>> parse (string "abc") ""
Failure ..
>>> parse (string "abc") "abcd"
Success "abc"
>>> parse (string "abc") "xabc"
Failure ..

==== Notes
* The error messages generated by `string` do not reflect how far into the input it managed
  to get: this is because the error being positioned at the start of the string is more
  natural. However, input __will__ still be consumed for purposes of backtracking.

@since 0.1.0.0
-}
string :: String        -- ^ the string, @s@, to be parsed from the input
       -> Parsec String -- ^ a parser that either parses the string @s@ or fails at the first
                        -- mismatched character.
string s = require (not (null s)) "Text.Gigaparsec.Char.string" "cannot pass empty string" $
  traverse char s

-------------------------------------------------
-- Composite Combinators
-------------------------------------------------

-- Could be optimised to remove the partiality
{-
TODO:

@since 0.1.0.0
-}
satisfyMap :: (Char -> Maybe a) -> Parsec a
satisfyMap f = fromJust . f <$> satisfy (isJust . f)

-- Use RangeSet internally?
-- the labelling here should be consistent with raw characters, not sure how
-- other than hard-coding it like Scala parsley?
{-|
This combinator tries to parse any character from supplied set of characters @cs@, returning it if
successful.

If the next character in the input is a member of the set @cs@, it is consumed and returned.
Otherwise, no input is consumed and the combinator fails.

==== __Examples__
>>> let p = oneOf (Set.fromList ['a'..'c'])
>>> parse p "a"
Success 'a'
>>> parse p "c"
Success 'c'
>>> parse p "xb"
Failure ..

@since 0.1.0.0
-}
oneOf :: Set Char    -- ^ the set of character, @cs@, to check.
      -> Parsec Char -- ^ a parser that parses one of the member of the set @cs@.
oneOf cs
  | sz == 0                     = empty
  | sz == 1                     = char c1
  -- if the smallest and largest characters are as far apart
  -- as the size of the set, it must be contiguous
  | sz == (ord c2 - ord c1 + 1) = satisfy (\c -> c1 <= c && c <= c2) <?> Set.mapMonotonic show cs
  | otherwise                   = satisfy (`Set.member` cs) <?> [rangeLabel]
  where !sz = Set.size cs
        -- must be left lazy until sz known not to be 0
        c1 = Set.findMin cs
        c2 = Set.findMax cs
        --FIXME: control character safe show (and for the map above!)
        rangeLabel = "one of " ++ show c1 ++ " to " ++ show c2

{-|
This combinator tries to parse any character __not__ from supplied set of characters @cs@,
returning it if successful.

If the next character in the input is not a member of the set @cs@, it is consumed and returned.
Otherwise, no input is consumed and the combinator fails.

==== __Examples__
>>> let p = noneOf (Set.from ['a'..'c'])
>>> parse p "a"
Failure ..
>>> parse p "c"
Failure ..
>>> parse p "xb"
Success 'x'
>>> parse p ""
Failure ..

@since 0.1.0.0
-}
noneOf :: Set Char    -- ^ the set, @cs@, of characters to check.
       -> Parsec Char -- ^ a parser that parses one character that is not a member of the set @cs@.
noneOf cs
  | sz == 0                     = item
  | sz == 1                     = satisfy (/= c1) <?> ["anything except " ++ show c1]
  | sz == (ord c2 - ord c1 + 1) = satisfy (\c -> c < c1 || c2 < c) <?> [rangeLabel]
  | otherwise                   = satisfy (not . (`Set.member` cs))
  where !sz = Set.size cs
        -- must be left lazy until sz known not to be 0
        c1 = Set.findMin cs
        c2 = Set.findMax cs
        --FIXME: control character safe show
        rangeLabel = "anything outside of " ++ show c1 ++ " to " ++ show c2

{-
TODO:

@since 0.1.0.0
-}
stringOfMany :: (Char -> Bool) -> Parsec String
stringOfMany p = many (satisfy p)

{-
TODO:

@since 0.1.0.0
-}
stringOfSome :: (Char -> Bool) -> Parsec String
stringOfSome p = some (satisfy p)

{-|
This combinator tries to parse each of the strings @strs@, until one of them succeeds.

Unlike `Text.Gigaparsec.Combinator.choice`, this combinator will not necessarily parse the strings in the
order provided. It will avoid strings that have another string as a prefix first, so that it has
/Longest Match/ semantics. It will try to minimise backtracking too, making it a much more efficient
option than @choice . map atomic@.

The longest succeeding string will be returned. If no strings match then the combinator fails.

==== __Examples__
>>> let p = strings (Set.fromList ["hell", "hello", "goodbye", "g", "abc"])
>>> parse p "hell"
Success "hell"
>>> parse p "hello"
Success "hello"
>>> parse p "good"
Success "g"
>>> parse p "goodbye"
Success "goodbye"
>>> parse p "a"
Failure ..

@since 0.1.0.0
-}
strings :: Set String    -- ^ the strings to try to parse.
        -> Parsec String -- ^ a parser that tries to parse all the given strings returning the
                         -- longest one that matches.
strings = _trie "Text.Gigaparsec.Char.strings" . Map.fromSet pure

-- Departure from original naming, but no overloading, so oh well
{-|
This combinator tries to parse each of the key-value pairs @kvs@, until one of them succeeds.

Each key-value pair in the map provided to this combinator is a string and a parser to perform if
that string can be parsed. Keys that are a prefix of another key are avoided, so that the parser
overall has /Longest Match/ semantics. It will try and minimise backtracking too, making it an
efficient option.

==== __Examples__
>>> let p = trie $ Map.fromList [ ("hell", pure 4)
                                , ("hello", pure 5)
                                , ("goodbye", pure 7)
                                , ("g", pure 1)
                                , ("abc", pure 3)
                                ]
>>> parse p "hell"
Success 4
>>> parse p "hello"
Success 5
>>> parse p "good"
Success 1
>>> parse p "goodbye"
Success 7
>>> parse p "a"
Failure ..

==== Notes
* The scope of any backtracking performed is isolated to the key itself, as it is assumed
  that once a key parses correctly, the branch has been committed to. Putting an `atomic`
  around the values will not affect this behaviour.

@since 0.1.0.0
-}
trie :: Map String (Parsec a) -- ^ the key-value pairs to try to parse.
     -> Parsec a              -- ^ a parser that tries to parse all the given key-value pairs,
                              -- returning the (possibly failing) result of the value that
                              -- corresponds to the longest matching key.
trie = _trie "Text.Gigaparsec.Char.trie"

_trie :: String -> Map String (Parsec a) -> Parsec a
_trie func strs = require (not (Map.member "" strs)) func "the empty string is not a valid key" $
  getAlt $ foldMap combineSameLeading (NonEmpty.groupWith (head . fst) (Map.toAscList strs))
  where -- When combining these parsers it is important to make sure the
        -- longest ones parse first. All but the last parser need an `atomic`.
        combineSameLeading :: NonEmpty (String, Parsec a) -> Alt Parsec a
        combineSameLeading group = foldMap (\(str, p) -> Alt $ atomic (string str) *> p) (reverse rest)
                               <|> Alt (string shortest *> shortP)
          where (shortest, shortP) :| rest = NonEmpty.sortWith (length . fst) group

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
spaces = skipMany space

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
whitespaces = skipMany whitespace

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

A digit is one of @\'0\'@ to @\'9\'@ (inclusive).

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
