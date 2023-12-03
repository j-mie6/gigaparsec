{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Gigaparsec.Token.Symbol (module Text.Gigaparsec.Token.Symbol) where

import Text.Gigaparsec (Parsec, void{-, notFollowedBy-})
import Text.Gigaparsec.Char (string)
import Text.Gigaparsec.Token.Descriptions ( SymbolDesc(SymbolDesc, hardKeywords, hardOperators)
                                          , NameDesc(NameDesc)
                                          )

import Data.Set qualified as Set (member)

type Symbol :: *
data Symbol = Symbol { softKeyword :: !(String -> Parsec ())
                     , softOperator :: !(String -> Parsec ())
                     }

mkSymbol :: SymbolDesc -> NameDesc -> Symbol
mkSymbol SymbolDesc{} NameDesc{} = Symbol { softKeyword = void . string  -- TODO:
                                          , softOperator = void . string -- TODO:
                                          }

mkSym :: SymbolDesc -> Symbol -> (String -> Parsec ())
mkSym SymbolDesc{..} Symbol{..} str
  | Set.member str hardKeywords  = softKeyword str
  | Set.member str hardOperators = softOperator str
  | otherwise                    = void (string str)

lexeme :: (forall a. Parsec a -> Parsec a) -> Symbol -> Symbol
lexeme _lex Symbol{..} = Symbol { softKeyword = _lex . softKeyword
                                , softOperator = _lex . softKeyword
                                }

-- TODO: HasField instances for the dot/comma/etc?
-- FIXME: to make these work, well need to move sym into Symbol?
{-dot :: Symbol -> Parsec ()
dot =
-}
