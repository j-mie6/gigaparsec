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

type Symbol :: *
data Symbol = Symbol { softKeyword :: !(String -> Parsec ())
                     , softOperator :: !(String -> Parsec ())
                     }

mkSymbol :: SymbolDesc -> NameDesc -> ErrorConfig -> Symbol
mkSymbol SymbolDesc{..} NameDesc{..} !err = Symbol {..}
  where softKeyword name = require (not (null name)) "softKeyword" "keywords may not be empty"
          (_softKeyword caseSensitive identifierLetter name err <?> [name])
        softOperator name = require (not (null name)) "softOperator" "operators may not be empty"
          (_softOperator hardOperators operatorLetter name err <?> [name])

mkSym :: SymbolDesc -> Symbol -> ErrorConfig -> (String -> Parsec ())
mkSym SymbolDesc{..} Symbol{..} !err str =
  annotate (Map.findWithDefault notConfigured str (labelSymbol err)) $
    if | Set.member str hardKeywords  -> softKeyword str
       | Set.member str hardOperators -> softOperator str
       | otherwise                    -> void (atomic (string str))

lexeme :: (forall a. Parsec a -> Parsec a) -> Symbol -> Symbol
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
