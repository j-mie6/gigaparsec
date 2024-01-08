{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Text.Gigaparsec.Token.Descriptions (module Text.Gigaparsec.Token.Descriptions) where

import Data.Char (isSpace)
import Data.Set (Set)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)

type LexicalDesc :: *
data LexicalDesc = LexicalDesc { nameDesc :: {-# UNPACK #-} !NameDesc
                               , symbolDesc :: {-# UNPACK #-} !SymbolDesc
                               , numericDesc :: {-# UNPACK #-} !NumericDesc
                               , textDesc :: {-# UNPACK #-} !TextDesc
                               , spaceDesc :: {-# UNPACK #-} !SpaceDesc
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
data NumericDesc = NumericDesc { literalBreakChar :: !BreakCharDesc
                               , leadingDotAllowed :: !Bool
                               , trailingDotAllowed :: !Bool
                               , leadingZerosAllowed :: !Bool
                               , positiveSign :: !PlusSignPresence
                               -- generic number
                               , integerNumbersCanBeHexadecimal :: !Bool
                               , integerNumbersCanBeOctal :: !Bool
                               , integerNumbersCanBeBinary :: !Bool
                               , realNumbersCanBeHexadecimal :: !Bool
                               , realNumbersCanBeOctal :: !Bool
                               , realNumbersCanBeBinary :: !Bool
                               -- special literals
                               , hexadecimalLeads :: !(Set Char)
                               , octalLeads :: !(Set Char)
                               , binaryLeads :: !(Set Char)
                               -- exponents
                               , decimalExponentDesc :: !ExponentDesc
                               , hexadecimalExponentDesc :: !ExponentDesc
                               , octalExponentDesc :: !ExponentDesc
                               , binaryExponentDesc :: !ExponentDesc
                               }

plainNumeric :: NumericDesc
plainNumeric = NumericDesc { literalBreakChar = NoBreakChar
                           , leadingDotAllowed = False
                           , trailingDotAllowed = False
                           , leadingZerosAllowed = True
                           , positiveSign = PlusOptional
                           -- generic number
                           , integerNumbersCanBeHexadecimal = True
                           , integerNumbersCanBeOctal = True
                           , integerNumbersCanBeBinary = False
                           , realNumbersCanBeHexadecimal = False
                           , realNumbersCanBeOctal = False
                           , realNumbersCanBeBinary = False
                           -- special literals
                           , hexadecimalLeads = ['x', 'X']
                           , octalLeads = ['o', 'O']
                           , binaryLeads = ['b', 'B']
                           -- exponents
                           , decimalExponentDesc = ExponentsSupported { compulsory = False
                                                                      , chars = ['e', 'E']
                                                                      , base = 10
                                                                      , expSign = PlusOptional
                                                                      }
                           , hexadecimalExponentDesc = ExponentsSupported { compulsory = True
                                                                          , chars = ['p', 'P']
                                                                          , base = 2
                                                                          , expSign = PlusOptional
                                                                          }
                           , octalExponentDesc = ExponentsSupported { compulsory = True
                                                                    , chars = ['e', 'E', 'p', 'P']
                                                                    , base = 2
                                                                    , expSign = PlusOptional
                                                                    }
                           , binaryExponentDesc = ExponentsSupported { compulsory = True
                                                                     , chars = ['e', 'E', 'p', 'P']
                                                                     , base = 2
                                                                     , expSign = PlusOptional
                                                                     }
                           }

type ExponentDesc :: *
data ExponentDesc = NoExponents
                  | ExponentsSupported { compulsory :: !Bool
                                       , chars :: !(Set Char)
                                       , base :: !Int
                                       , expSign :: !PlusSignPresence
                                       }

type BreakCharDesc :: *
data BreakCharDesc = NoBreakChar
                   | BreakCharSupported { breakChar :: !Char
                                        , allowedAfterNonDecimalPrefix :: !Bool
                                        }

type PlusSignPresence :: *
data PlusSignPresence = PlusRequired | PlusOptional | PlusIllegal

type TextDesc :: *
data TextDesc = TextDesc { escapeSequences :: {-# UNPACK #-} !EscapeDesc
                         , characterLiteralEnd :: !Char
                         , stringEnds :: !(Set String)
                         , multiStringEnds :: !(Set String)
                         , graphicCharacter :: !CharPredicate
                         }

plainText :: TextDesc
plainText = TextDesc { escapeSequences = plainEscape
                     , characterLiteralEnd = '\''
                     , stringEnds = ["\""]
                     , multiStringEnds = []
                     , graphicCharacter = Just (>= ' ')
                     }

type EscapeDesc :: *
data EscapeDesc = EscapeDesc { escBegin :: !Char
                             , literals :: !(Set Char)
                             , singleMap :: !(Map Char Char)
                             , multiMap :: !(Map String Char)
                             , decimalEscape :: !NumericEscape
                             , hexadecimalEscape :: !NumericEscape
                             , octalEscape :: !NumericEscape
                             , binaryEscape :: !NumericEscape
                             , emptyEscape :: !(Maybe Char)
                             , gapsSupported :: !Bool
                             }

plainEscape :: EscapeDesc
plainEscape = EscapeDesc { escBegin = '\\'
                         , literals = ['\\']
                         , singleMap = []
                         , multiMap = []
                         , decimalEscape = NumericIllegal
                         , hexadecimalEscape = NumericIllegal
                         , octalEscape = NumericIllegal
                         , binaryEscape = NumericIllegal
                         , emptyEscape = Nothing
                         , gapsSupported = False
                         }

-- TODO: haskellEscape

type NumericEscape :: *
data NumericEscape = NumericIllegal
                   | NumericSupported { prefix :: !(Maybe Char)
                                      , numDigits :: !NumberOfDigits
                                      , maxValue :: !Int
                                      }

type NumberOfDigits :: *
data NumberOfDigits = Unbounded | Exactly !(NonEmpty Word) | AtMost !Word

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
