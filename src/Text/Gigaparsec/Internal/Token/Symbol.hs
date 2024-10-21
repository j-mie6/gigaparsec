{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards, OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Symbol (
    Symbol, softKeyword, softOperator, mkSymbol, mkSym, lexeme
  ) where

import Text.Gigaparsec (Parsec, void, notFollowedBy, atomic, (<|>), empty)
import Text.Gigaparsec.Char (string, satisfy, char, strings)
import Text.Gigaparsec.Token.Descriptions ( SymbolDesc(SymbolDesc, hardKeywords, hardOperators, caseSensitive)
                                          , NameDesc(NameDesc, identifierLetter, operatorLetter)
                                          , CharPredicate
                                          )

import Data.Set qualified as Set (member, toList, fromList, null)
import Data.Char (toUpper, toLower, isLetter)
import Text.Gigaparsec.Errors.Combinator (amend, emptyWide, (<?>), label)
import Data.Set (Set)
import Data.Map qualified as Map (findWithDefault)
import Data.Maybe (mapMaybe)
import Text.Gigaparsec.Internal.Require (require)
import Text.Gigaparsec.Token.Errors (ErrorConfig (labelSymbolEndOfKeyword, labelSymbolEndOfOperator, labelSymbol), notConfigured)
import Text.Gigaparsec.Internal.Token.Errors (annotate)

{-|
This contains lexing functionality relevant to the parsing of atomic symbols.

Symbols are characterised by their "unitness", that is, every parser inside returns Unit. 
This is because they all parse a specific known entity, and, as such, the result of the parse is irrelevant. 
These can be things such as reserved names, or small symbols like parentheses. 

This type also contains a means of creating new symbols as well as implicit conversions 
to allow for Haskell's string literals (with @OverloadedStringLiterals@ enabled) to serve as symbols within a parser.
-}
type Symbol :: *
data Symbol = Symbol { 
  {- | This combinator parses a given soft keyword atomically: 
  the keyword is only valid if it is not followed directly by a character 
  which would make it a larger valid identifier.

  Soft keywords are keywords that are only reserved within certain contexts. 
  The 'Text.Gigaparsec.Token.Lexer.apply' combinator handles so-called hard keywords automatically, 
  as the given string is checked to see what class of symbol it might belong to.
  However, soft keywords are not included in this set, 
  as they are not always reserved in all situations. 
  As such, when a soft keyword does need to be parsed, 
  this combinator should be used to do it explicitly. 
  Care should be taken to ensure that soft keywords take
  parsing priority over identifiers when they do occur.
  -}
    softKeyword :: !(String -> Parsec ())
  {-|
  This combinator parses a given soft operator atomically:
  the operator is only valid if it is not followed directly by a character which 
  would make it a larger valid operator (reserved or otherwise).

  Soft operators are operators that are only reserved within certain contexts. 
  The apply combinator handles so-called hard operators automatically, 
  as the given string is checked to see what class of symbol it might belong to. 
  However, soft operators are not included in this set, 
  as they are not always reserved in all situations.
  As such, when a soft operator does need to be parsed, 
  this combinator should be used to do it explicitly.
  -}
  , softOperator :: !(String -> Parsec ())
  }

{-|
Create a 'Symbol' -- an interface for parsing atomic symbols -- according to the given descriptions.
-}
mkSymbol  :: SymbolDesc  -- ^ the description of symbols (keywords and operators).
          -> NameDesc    -- ^ the description of identifiers.
          -> ErrorConfig -- ^ how errors should be produced on failed parses.
          -> Symbol      -- ^ a collection of parsers for keywords and operators as described by the given descriptions.
mkSymbol SymbolDesc{..} NameDesc{..} !err = Symbol {..}
  where softKeyword name = require (not (null name)) "softKeyword" "keywords may not be empty"
          (_softKeyword caseSensitive identifierLetter name err <?> [name])
        softOperator name = require (not (null name)) "softOperator" "operators may not be empty"
          (_softOperator hardOperators operatorLetter name err <?> [name])

{-|
Create a parser that parses the given string, but first checks if this is a hard keyword or operator, 
in which case these are parsed using the 'softKeyword' and 'softOperator' parsers instead.
-}
mkSym :: SymbolDesc             -- ^ @symbolDesc@, the description of symbols (keywords and operators)
      -> Symbol                 -- ^ @symbol@, a collection of parsers for symbols described by @symbolDesc@
      -> ErrorConfig            -- ^ how errors should be produced on failed parses.
      -> (String -> Parsec ())  -- ^ a function which parses the given string; if this string is a keyword or operator, 
                                -- ^ it is parsed using @symbol@.
mkSym SymbolDesc{..} Symbol{..} !err str =
  annotate (Map.findWithDefault notConfigured str (labelSymbol err)) $
    if | Set.member str hardKeywords  -> softKeyword str
       | Set.member str hardOperators -> softOperator str
       | otherwise                    -> void (atomic (string str))

{-|
Given a whitespace consumer (or any function on a parser) and a 'Symbol' parser, create a 'Symbol' parser
where each constituent parser also applies this whitespace consumer after parsing.
-}
lexeme  :: (forall a. Parsec a -> Parsec a) -- ^ @f@, a parser transformer, usually consumes whitespace after running the parser
        -> Symbol -- ^ the symbol parser
        -> Symbol -- ^ a symbol parser with each constituent parser transformed by @f@.
lexeme lexe Symbol{..} = Symbol { softKeyword = lexe . softKeyword
                                , softOperator = lexe . softOperator
                                }

_softKeyword :: Bool -> CharPredicate -> String -> ErrorConfig -> Parsec ()
_softKeyword !caseSensitive !letter !kw !err
  | not caseSensitive = atomic (nfb letter caseString) <?> [kw]
  | otherwise         = atomic (nfb letter (string kw)) <?> [kw]
  where nfb Nothing p = void p
        nfb (Just c) p = p *> (notFollowedBy (satisfy c) <?> [labelSymbolEndOfKeyword err kw])
        n = length kw
        caseChar c
          | isLetter c = char (toUpper c) <|> char (toLower c)
          | otherwise  = char c
        caseString = atomic (amend (traverse caseChar kw))
                 <|> emptyWide (fromIntegral n)

-- TODO: trie-based implementation
_softOperator :: Set String -> CharPredicate -> String -> ErrorConfig -> Parsec ()
_softOperator !hardOperators !letter !op !err = label [op] $
  if Set.null ends then atomic (string op *> notFollowedBy letter')
  else atomic (string op *> (notFollowedBy (void letter' <|> void (strings ends)) <?> [labelSymbolEndOfOperator err op]))
  where ends = Set.fromList (mapMaybe (flip strip op) (Set.toList hardOperators))
        letter' = maybe empty satisfy letter
        strip []      str@(:){}          = Just str
        strip (c:pre) (c':str) | c == c' = strip pre str
        strip _       _                  = Nothing

-- TODO: HasField instances for the dot/comma/etc?
-- FIXME: to make these work, well need to move sym into Symbol?
{-dot :: Symbol -> Parsec ()
dot =
-}
