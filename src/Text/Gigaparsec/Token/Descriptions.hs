{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
module Text.Gigaparsec.Token.Descriptions (module Text.Gigaparsec.Token.Descriptions) where

import Data.Char (isSpace)
import Data.Set (Set)

type LexicalDesc :: *
data LexicalDesc = LexicalDesc { nameDesc :: !NameDesc
                               , symbolDesc :: !SymbolDesc
                               , numericDesc :: !NumericDesc
                               , textDesc :: !TextDesc
                               , spaceDesc :: !SpaceDesc
                               }

plain :: LexicalDesc
plain = LexicalDesc { nameDesc = plainName
                    , symbolDesc = plainSymbol
                    , numericDesc = plainNumeric
                    , textDesc = plainText
                    , spaceDesc = plainSpace
                    }

type NameDesc :: *
data NameDesc = NameDesc { identifierStart :: !CharPredicate
                         , identifierLetter :: !CharPredicate
                         , operatorStart :: !CharPredicate
                         , operatorLetter :: !CharPredicate
                         }

plainName :: NameDesc
plainName = NameDesc { identifierStart = Nothing
                     , identifierLetter = Nothing
                     , operatorStart = Nothing
                     , operatorLetter = Nothing
                     }

type SymbolDesc :: *
data SymbolDesc = SymbolDesc { hardKeywords :: !(Set String)
                             , hardOperators :: !(Set String)
                             , caseSensitive :: !Bool
                             }

plainSymbol :: SymbolDesc
plainSymbol = SymbolDesc { hardKeywords = []
                         , hardOperators = []
                         , caseSensitive = True
                         }

type NumericDesc :: *
data NumericDesc = NumericDesc {}

plainNumeric :: NumericDesc
plainNumeric = NumericDesc {}

type TextDesc :: *
data TextDesc = TextDesc {}

plainText :: TextDesc
plainText = TextDesc {}

type SpaceDesc :: *
data SpaceDesc = SpaceDesc { commentStart :: !String
                           , commentEnd :: !String
                           , commentLine :: !String
                           , commentLineAllowsEOF :: !Bool
                           , nestedComments :: !Bool
                           , space :: !CharPredicate
                           , whitespaceIsContextDependent :: !Bool
                           }

plainSpace :: SpaceDesc
plainSpace = SpaceDesc { commentStart = ""
                       , commentEnd = ""
                       , commentLine = ""
                       , commentLineAllowsEOF = True
                       , nestedComments = False
                       , space = Just isSpace
                       , whitespaceIsContextDependent = False
                       }

type CharPredicate :: *
type CharPredicate = Maybe (Char -> Bool)
