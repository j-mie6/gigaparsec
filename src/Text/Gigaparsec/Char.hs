{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
-- needs to be explicit for the docs later
module Text.Gigaparsec.Char (module Text.Gigaparsec.Char) where

-- We want to use this to make the docs point to the right definition for users.
import Text.Gigaparsec (Parsec, (<?>), atomic)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec))
import Text.Gigaparsec.Internal.Require (require)

import Control.Applicative (empty, (<|>), many)
import Data.Char (ord)
import Data.Char qualified as Char
import Data.Functor (void)
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

satisfy :: (Char -> Bool) -> Parsec Char
satisfy _ = Internal.Parsec undefined --TODO:

-- Needs to be primitive for the raw expected item down the line
char :: Char -> Parsec Char
char c = satisfy (== c)

-- Needs to be primitive for the raw expected item and wide caret down the line
string :: String -> Parsec String
string s = require (not (null s)) "Cannot pass empty string to `string`" $
  traverse char s

-------------------------------------------------
-- Composite Combinators
-------------------------------------------------

-- Could be optimised to remove the partiality
satisfyMap :: (Char -> Maybe a) -> Parsec a
satisfyMap f = fromJust . f <$> satisfy (isJust . f)

-- Use RangeSet internally?
-- the labelling here should be consistent with raw characters, not sure how
-- other than hard-coding it like Scala parsley?
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

noneOf :: Set Char -> Parsec Char
noneOf cs
  | sz == 0                 = item
  | sz == 1                 = satisfy (c1 /=) <?> ["anything except " ++ show c1]
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

strings :: Set String -> Parsec String
strings = trie . Map.fromSet pure

-- Departure from original naming, but no overloading, so oh well
trie :: Map String (Parsec a) -> Parsec a
trie strs = require (not (Map.member "" strs)) "Cannot pass empty string to `strings` or `trie`" $
  getAlt $ foldMap combineSameLeading (groupWith (head . fst) (Map.toAscList strs))
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

item :: Parsec Char
item = satisfy (const True) <?> ["any character"]

space :: Parsec Char
space = satisfy (\c -> c == ' ' || c == '\t') <?> ["space", "tab"]

spaces :: Parsec ()
spaces = void (many space) -- TODO: skipMany

whitespace :: Parsec Char
whitespace = satisfy Char.isSpace <?> ["whitespace"]

whitespaces :: Parsec ()
whitespaces = void (many whitespace) -- TODO: skipMany

newline :: Parsec Char
newline = char '\n' <?> ["newline"]

crlf :: Parsec Char
crlf = '\n' <$ atomic (string "\r\n") <?> ["crlf"]

endOfLine :: Parsec Char
endOfLine = newline <|> crlf <?> ["end of line"]

tab :: Parsec Char
tab = char '\t' <?> ["tab"]

upper :: Parsec Char
upper = satisfy Char.isUpper <?> ["uppercase letter"]

lower :: Parsec Char
lower = satisfy Char.isLower <?> ["lowercase letter"]

letterOrDigit :: Parsec Char
letterOrDigit = satisfy Char.isAlphaNum <?> ["alpha-numeric character"]

letter :: Parsec Char
letter = satisfy Char.isAlpha <?> ["letter"]

digit :: Parsec Char
digit = satisfy Char.isDigit <?> ["digit"]

hexDigit :: Parsec Char
hexDigit = satisfy Char.isHexDigit <?> ["hexadecimal digit"]

octDigit :: Parsec Char
octDigit = satisfy Char.isOctDigit <?> ["octal digit"]

bit :: Parsec Char
bit = oneOf ['0', '1'] <?> ["bit"]
