{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards, OverloadedLists #-}
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
import Text.Gigaparsec.Errors.Combinator (amend, emptyWide, (<?>))
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Text.Gigaparsec.Internal.Require (require)

type Symbol :: *
data Symbol = Symbol { softKeyword :: !(String -> Parsec ())
                     , softOperator :: !(String -> Parsec ())
                     }

mkSymbol :: SymbolDesc -> NameDesc -> Symbol
mkSymbol SymbolDesc{..} NameDesc{..} = Symbol {..}
  where softKeyword name = require (not (null name)) "softKeyword" "keywords may not be empty"
          (_softKeyword caseSensitive identifierLetter name <?> [name])
        softOperator name = require (not (null name)) "softOperator" "operators may not be empty"
          (_softOperator hardOperators operatorLetter name <?> [name])

mkSym :: SymbolDesc -> Symbol -> (String -> Parsec ())
mkSym SymbolDesc{..} Symbol{..} str
  | Set.member str hardKeywords  = softKeyword str
  | Set.member str hardOperators = softOperator str
  | otherwise                    = void (atomic (string str))

lexeme :: (forall a. Parsec a -> Parsec a) -> Symbol -> Symbol
lexeme lexe Symbol{..} = Symbol { softKeyword = lexe . softKeyword
                                , softOperator = lexe . softOperator
                                }

_softKeyword :: Bool -> CharPredicate -> String -> Parsec ()
_softKeyword caseSensitive letter kw
  | caseSensitive = atomic (nfb letter caseString)
  | otherwise     = atomic (nfb letter (string kw))
  where nfb Nothing p = void p
        nfb (Just c) p = p *> (notFollowedBy (satisfy c) <?> ["end of " ++ kw])
        n = length kw
        caseChar c
          | isLetter c = char (toUpper c) <|> char (toLower c)
          | otherwise  = char c
        caseString = atomic (amend (traverse caseChar kw))
                 <|> emptyWide (fromIntegral n)

-- TODO: trie-based implementation
_softOperator :: Set String -> CharPredicate -> String -> Parsec ()
_softOperator hardOperators letter op =
  if Set.null ends then atomic (string op *> notFollowedBy letter')
  else atomic (string op *> (notFollowedBy (void letter' <|> void (strings ends)) <?> ["end of " ++ op]))
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
