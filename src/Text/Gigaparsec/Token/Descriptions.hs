{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Token.Descriptions (module Text.Gigaparsec.Token.Descriptions) where

type LexicalDesc :: *
data LexicalDesc = LexicalDesc { nameDesc :: !NameDesc
                               , symbolDesc :: !SymbolDesc
                               , numericDesc :: !NumericDesc
                               , textDesc :: !TextDesc
                               , spaceDesc :: !SpaceDesc
                               }

type NameDesc :: *
data NameDesc = NameDesc {}

type SymbolDesc :: *
data SymbolDesc = SymbolDesc {}

type NumericDesc :: *
data NumericDesc = NumericDesc {}

type TextDesc :: *
data TextDesc = TextDesc {}

type SpaceDesc :: *
data SpaceDesc = SpaceDesc { commentStart :: !String
                           , commentEnd :: !String
                           , commentLine :: !String
                           , commentLineAllowsEOF :: !Bool
                           , nestedComments :: !Bool
                           , space :: !CharPredicate
                           , whitespaceIsContextDependent :: !Bool
                           }

type CharPredicate :: *
type CharPredicate = Maybe (Char -> Bool)
