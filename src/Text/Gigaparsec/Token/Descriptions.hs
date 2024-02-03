{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- TODO: In next major, don't expose the constructors of the descriptions,
-- we want them built up by record copy for forwards compatible evolution
-- We can move this into an internal module to accommodate that if we want
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
                                                                      , expLeadingZerosAllowd = True
                                                                      }
                           , hexadecimalExponentDesc = ExponentsSupported { compulsory = True
                                                                          , chars = ['p', 'P']
                                                                          , base = 2
                                                                          , expSign = PlusOptional
                                                                          , expLeadingZerosAllowd = True
                                                                          }
                           , octalExponentDesc = ExponentsSupported { compulsory = True
                                                                    , chars = ['e', 'E', 'p', 'P']
                                                                    , base = 2
                                                                    , expSign = PlusOptional
                                                                    , expLeadingZerosAllowd = True
                                                                    }
                           , binaryExponentDesc = ExponentsSupported { compulsory = True
                                                                     , chars = ['e', 'E', 'p', 'P']
                                                                     , base = 2
                                                                     , expSign = PlusOptional
                                                                     , expLeadingZerosAllowd = True
                                                                     }
                           }

type ExponentDesc :: *
data ExponentDesc = NoExponents
                  | ExponentsSupported { compulsory :: !Bool
                                       , chars :: !(Set Char)
                                       , base :: !Int
                                       , expSign :: !PlusSignPresence
                                       , expLeadingZerosAllowd :: !Bool
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
                         , stringEnds :: !(Set (String, String))
                         , multiStringEnds :: !(Set (String, String))
                         , graphicCharacter :: !CharPredicate
                         }

plainText :: TextDesc
plainText = TextDesc { escapeSequences = plainEscape
                     , characterLiteralEnd = '\''
                     , stringEnds = [("\"", "\"")]
                     , multiStringEnds = []
                     , graphicCharacter = Just (>= ' ')
                     }

type EscapeDesc :: *
data EscapeDesc = EscapeDesc { escBegin :: !Char
                             , literals :: !(Set Char)
                             , mapping :: !(Map String Char)
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
                         , mapping = []
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
                                      , maxValue :: !Char
                                      }

type NumberOfDigits :: *
data NumberOfDigits = Unbounded | Exactly !(NonEmpty Word) | AtMost !Word

type SpaceDesc :: *
data SpaceDesc = SpaceDesc { lineCommentStart :: !String
                           , lineCommentAllowsEOF :: !Bool
                           , multiLineCommentStart :: !String
                           , multiLineCommentEnd :: !String
                           , multiLineNestedComments :: !Bool
                           , space :: !CharPredicate
                           , whitespaceIsContextDependent :: !Bool
                           }

plainSpace :: SpaceDesc
plainSpace = SpaceDesc { lineCommentStart = ""
                       , lineCommentAllowsEOF = True
                       , multiLineCommentStart = ""
                       , multiLineCommentEnd = ""
                       , multiLineNestedComments = False
                       , space = Just isSpace
                       , whitespaceIsContextDependent = False
                       }

type CharPredicate :: *
type CharPredicate = Maybe (Char -> Bool)
