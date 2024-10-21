{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Names (
    Names, mkNames,
    identifier, identifier',
    userDefinedOperator, userDefinedOperator',
    lexeme
  ) where

import Text.Gigaparsec (Parsec, empty, (<:>), atomic)
import Text.Gigaparsec.Char (stringOfMany, satisfy)
import Text.Gigaparsec.Errors.Combinator ((<?>), unexpectedWhen)

import Data.Set qualified as Set (member, map)

import Text.Gigaparsec.Token.Descriptions (
    SymbolDesc(SymbolDesc, hardKeywords, hardOperators, caseSensitive),
    NameDesc(NameDesc, identifierStart, identifierLetter,
                       operatorStart, operatorLetter),
    CharPredicate
  )
import Data.Char (toLower)
import Text.Gigaparsec.Token.Errors (
    ErrorConfig (labelNameIdentifier, unexpectedNameIllegalIdentifier, labelNameOperator, unexpectedNameIllegalOperator, filterNameIllFormedIdentifier, filterNameIllFormedOperator)
  )
import Text.Gigaparsec.Internal.Token.Errors (filterS)

-- TODO: primes are gross, better way?
{-|
This class defines a uniform interface for defining parsers for user-defined names 
(identifiers and operators), independent of how whitespace should be handled after the name.

The parsing of names is mostly concerned with finding the longest valid name that is not a reserved name, 
such as a hard keyword or a special operator.
-}
type Names :: *
data Names = Names { 
  {-| 
  Parse an identifier based on the given 'NameDesc' predicates 'identifierStart' and 'identifierLetter'.
  The 'NameDesc' is provided by 'mkNames'.

  Capable of handling unicode characters if the configuration permits.
  If hard keywords are specified by the configuration, this parser is not permitted to parse them.
  -}
    identifier :: !(Parsec String)
  {-| 
  Parse an identifier whose start satisfies the given predicate, and subseqeunt letters satisfy 'identifierLetter' in the given 'NameDesc'.
  The 'NameDesc' is provided by 'mkNames'.

  Behaves as 'identifier', then ensures the first character matches the given predicate.
  Thus, 'identifier'' can only /refine/ the output of 'identifier';
  if 'identifier' fails due to the first character, then so will 'identifier'', 
  even if this character passes the supplied predicate.
  
  Capable of handling unicode characters if the configuration permits.
  If hard keywords are specified by the configuration, this parser is not permitted to parse them.
  -}
  , identifier' :: !(CharPredicate -> Parsec String)
  {-| 
  Parse a user-defined operator based on the given 'SymbolDesc' predicates 'operatorStart' and 'operatorLetter'.
  The 'SymbolDesc' is provided by 'mkNames'.

  Capable of handling unicode characters if the configuration permits. 
  If hard operators are specified by the configuration, this parser is not permitted to parse them.
  -}
  , userDefinedOperator :: !(Parsec String)
  {-| 
  Parse a user-defined operator whose first character satisfies the given predicate,
  and subsequent characters satisfying 'operatorLetter' in the given 'SymbolDesc'.
  The 'SymbolDesc' is provided by 'mkNames'.

  Behaves as 'userDefinedOperator', then ensures the first character matches the given predicate.
  Thus, 'userDefinedOperator'' can only /refine/ the output of 'userDefinedOperator';
  if 'userDefinedOperator' fails due to the first character, then so will 'userDefinedOperator'', 
  even if this character passes the supplied predicate.

  Capable of handling unicode characters if the configuration permits. 
  If hard operators are specified by the configuration, this parser is not permitted to parse them.
  -}
  , userDefinedOperator' :: !(CharPredicate -> Parsec String)
  }

{-|
Create a 'Names' -- an interface for parsing identifiers and operators 
-- according to the given name and symbol descriptions.
-}
mkNames :: NameDesc    -- ^ the description of identifiers.
        -> SymbolDesc  -- ^ the description of operators.
        -> ErrorConfig -- ^ how errors should be produced on failed parses.
        -> Names       -- ^ a collection of parsers for identifiers and operators as described by the given descriptions.
mkNames NameDesc{..} symbolDesc@SymbolDesc{..} !err = Names {..}
  where
    !isReserved = isReservedName symbolDesc
    !identifier =
      keyOrOp identifierStart identifierLetter isReserved (labelNameIdentifier err) (unexpectedNameIllegalIdentifier err)
    identifier' start = filterS (filterNameIllFormedIdentifier err) (startsWith start) identifier
    !userDefinedOperator =
      keyOrOp operatorStart operatorLetter (flip Set.member hardOperators) (labelNameOperator err) (unexpectedNameIllegalOperator err)
    userDefinedOperator' start = filterS (filterNameIllFormedOperator err) (startsWith start) userDefinedOperator

    keyOrOp :: CharPredicate -> CharPredicate -> (String -> Bool) -> String -> (String -> String) -> Parsec String
    keyOrOp start letter illegal name unexpectedIllegal =
      atomic (unexpectedWhen cond (complete start letter)) <?> [name]
      where cond x
              | illegal x = Just (unexpectedIllegal x)
              | otherwise = Nothing

    trailer :: CharPredicate -> Parsec String
    trailer = maybe (pure "") stringOfMany

    complete :: CharPredicate -> CharPredicate -> Parsec String
    complete (Just start) letter = satisfy start <:> trailer letter
    complete Nothing _ = empty

    startsWith :: CharPredicate -> String -> Bool
    startsWith Nothing _ = True
    startsWith (Just _) [] = False
    startsWith (Just p) (c:_) = p c

lexeme :: (forall a. Parsec a -> Parsec a) -> Names -> Names
lexeme lexe Names{..} = Names { identifier = lexe identifier
                              , identifier' = lexe . identifier'
                              , userDefinedOperator = lexe userDefinedOperator
                              , userDefinedOperator' = lexe . userDefinedOperator'
                              }

isReservedName :: SymbolDesc -> String -> Bool
isReservedName SymbolDesc{..}
  | caseSensitive = flip Set.member hardKeywords
  | otherwise     = flip Set.member lowerHardKeywords . allLower
  where allLower = map toLower
        lowerHardKeywords = Set.map allLower hardKeywords
