{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Token.Descriptions (module Text.Gigaparsec.Token.Descriptions) where
import Data.Char (isSpace)

type LexicalDesc :: *
data LexicalDesc = LexicalDesc { nameDesc :: !NameDesc
                               , symbolDesc :: !SymbolDesc
                               , numericDesc :: !NumericDesc
                               , textDesc :: !TextDesc
                               , spaceDesc :: !SpaceDesc
                               }

plain :: LexicalDesc
plain = LexicalDesc { nameDesc = NameDesc {}
                    , symbolDesc = SymbolDesc {}
                    , numericDesc = NumericDesc {}
                    , textDesc = TextDesc {}
                    , spaceDesc = plainSpace
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
