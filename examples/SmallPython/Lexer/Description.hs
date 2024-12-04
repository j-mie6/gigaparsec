{-# LANGUAGE OverloadedLists #-}

{-|
This module defines the lexical description for a subset of Python.
-}
module SmallPython.Lexer.Description (
  pythonDesc,
  isKeyword,
  isReservedOperator,
  reservedBinOps,
  reservedOperators,
  reservedUnaryOps,
  keywords,
  isNormalWhiteSpace,
  isParenWhiteSpace
  ) where


import Text.Gigaparsec

import Text.Gigaparsec.Token.Lexer qualified as Lexer
import Text.Gigaparsec.Token.Descriptions qualified as Desc

import GHC.Unicode (isAlpha, isAlphaNum, isSpace, isPrint)
import Data.Set qualified as Set
import Data.Set (Set)

---------------------------------------------------------------------------------------------------
-- Lexical Description

-- | The lexical description for a subset of Python.
-- This defines things such as keywords, identifiers, comments, etc.
pythonDesc :: Desc.LexicalDesc
pythonDesc = Desc.plain {
    Desc.nameDesc = nameDesc,
    Desc.symbolDesc = symbolDesc,
    Desc.numericDesc = numericDesc,
    Desc.spaceDesc = spaceDesc,
    Desc.textDesc = textDesc
  }

---------------------------------------------------------------------------------------------------
-- Name Description

-- | The characters that start a variable/identifier name
{-# inline identifierStart #-}
identifierStart :: Char -> Bool
identifierStart c = c == '_' || isAlpha c

-- | The characters that continue an identifier after the initial one in `identifierStart`.
{-# inline identifierLetter #-}
identifierLetter :: Char -> Bool
identifierLetter c = c == '_' || isAlphaNum c

-- | Lexical description of identifiers (variables, function names, etc...).
nameDesc :: Desc.NameDesc
nameDesc = Desc.plainName {
    Desc.identifierStart  = Just identifierStart,
    Desc.identifierLetter = Just identifierLetter,
    -- Users cannot define custom operators in Python,
    -- they must all be from one of the pre-defined operators:
    -- https://docs.python.org/3/library/operator.html#mapping-operators-to-functions
    Desc.operatorStart    = Nothing,
    Desc.operatorLetter   = Nothing
  }


---------------------------------------------------------------------------------------------------
-- Symbol Description

{-# inline isKeyword #-}
isKeyword :: String -> Bool
isKeyword = (`Set.member` keywords)

keywords :: Set String
keywords = [
  -- Definitions
  "def",
  "class",
  "lambda",
  -- Control Flow
  "return",
    -- If-else
    "if", "elif", "else",
    -- Loops
    "for",
    "in",
    "while",
    "continue",
    "break",
    -- Exceptions
    "try",
    "except",
    "finally",
    "raise",

  -- Atoms
  "None",
  "False",
  "True",
  -- boolean ops
  "and",
  "not",
  "or",

  -- module
  "global",
  "from",
  "import",
  "as",

  -- Others
  "assert",
  "async",
  "await",
  "del",
  "is",
  "nonlocal",
  "pass",
  "with",
  "yield"
  ]

{-# inline isReservedOperator #-}
isReservedOperator :: String -> Bool
isReservedOperator = (`Set.member` reservedOperators)

reservedBinOps :: Set String
reservedBinOps = [
  -- Arithmetic Symbols
  "+",
  "-",
  "/",
  "//",
  "%",
  "@",
  -- Bit arithmetic
  "&",
  "^",
  "~",
  "|",
  "**",
  "<<",
  ">>",
  -- Boolean checks
  "<",
  "<=",
  "==",
  "!=",
  ">=",
  ">"
  ]

reservedUnaryOps :: Set String
reservedUnaryOps = [
  -- Signed numbers
  "+",
  "-",
  -- Keywords as ops
  "del",
  "is",
  "not"
  ]

reservedOperators :: Set String
reservedOperators = Set.unions @Set [
  reservedUnaryOps, 
  reservedBinOps,
  [
    -- Grammatical
    ":", 
    ",",
    -- Assignments
    "=",
    "+=",
    "-=",
    "*=",
    "@=",
    "/=",
    "%=",
    "&=",
    "|=",
    "^=",
    "<<=",
    ">>=",
    "**=",
    "//="
    ]
  ]

symbolDesc :: Desc.SymbolDesc
symbolDesc = Desc.plainSymbol {
    Desc.hardKeywords = keywords,
    Desc.hardOperators = reservedOperators,
    Desc.caseSensitive = True
  }

---------------------------------------------------------------------------------------------------
-- Numeric Description

numericDesc :: Desc.NumericDesc
numericDesc = Desc.plainNumeric {
    Desc.literalBreakChar = breakCharDesc,
    Desc.leadingDotAllowed = True,
    Desc.trailingDotAllowed = True,
    Desc.leadingZerosAllowed = False,

    Desc.positiveSign = Desc.PlusOptional,
    
    Desc.integerNumbersCanBeBinary = True,
    Desc.integerNumbersCanBeHexadecimal = True,
    Desc.integerNumbersCanBeOctal = True,

    Desc.realNumbersCanBeBinary = False,
    Desc.realNumbersCanBeOctal = False,
    Desc.realNumbersCanBeHexadecimal = False,

    Desc.octalLeads = ['o', 'O'],
    Desc.hexadecimalLeads = ['x', 'X'],
    Desc.binaryLeads = ['b', 'B'],

    Desc.decimalExponentDesc = decimalExponentDesc,
    Desc.hexadecimalExponentDesc = Desc.NoExponents,
    Desc.binaryExponentDesc = Desc.NoExponents,
    Desc.octalExponentDesc = Desc.NoExponents
  }

decimalExponentDesc :: Desc.ExponentDesc
decimalExponentDesc = Desc.ExponentsSupported {
    compulsory = False,
    chars = ['e', 'E'],
    base = 10,
    expSign = Desc.PlusOptional,
    expLeadingZerosAllowd = True
  }

breakCharDesc :: Desc.BreakCharDesc
breakCharDesc = Desc.BreakCharSupported { 
    Desc.breakChar = '_'
  , Desc.allowedAfterNonDecimalPrefix = True 
  }

---------------------------------------------------------------------------------------------------
-- Space Description

spaceDesc :: Desc.SpaceDesc
spaceDesc = Desc.plainSpace {
    Desc.lineCommentStart = "#",
    Desc.lineCommentAllowsEOF = True,
    -- Python technically does not have multi-line comments, 
    -- they are just strings that are forgotten after processing.
    -- We can't let e.g. `"""` start and end comments, as we would then also be 
    -- ignoring triple-quoted strings!
    Desc.multiLineCommentStart = "",
    Desc.multiLineCommentEnd   = "",
    Desc.multiLineNestedComments = False,
    Desc.space = Just isNormalWhiteSpace,
    Desc.whitespaceIsContextDependent = True
  }

{-# inline isNormalWhiteSpace #-}
isNormalWhiteSpace :: Char -> Bool
isNormalWhiteSpace c = 
      c == ' ' 
  ||  c == '\f' 
  ||  c == '\v' 
  -- tabs are allowed after the first significant character in a line.
  ||  c == '\t'

-- | Inside parens, newlines are allowed.
{-# inline isParenWhiteSpace #-}
isParenWhiteSpace :: Char -> Bool
isParenWhiteSpace = isSpace

---------------------------------------------------------------------------------------------------
-- Text Description

textDesc :: Desc.TextDesc
textDesc = Desc.plainText {
    Desc.escapeSequences = escapeDesc,
    Desc.characterLiteralEnd = '\'',
    Desc.stringEnds = [
      ("\'", "\'"),
      ("\"", "\"")
      ],
    Desc.multiStringEnds = [
      ("\'\'\'", "\'\'\'"),
      ("\"\"\"", "\"\"\"")
      ],
    Desc.graphicCharacter = Just isPrint
  }

escapeDesc :: Desc.EscapeDesc
escapeDesc = Desc.plainEscape {
    Desc.escBegin = '\\',
    Desc.literals = ['\'', '\\'],
    Desc.mapping = [
      ("n", '\n'),
      ("r", '\r'),
      ("t", '\t'),
      ("b", '\b'),
      ("f", '\f')
    ],
    Desc.octalEscape = octalEscape,
    Desc.hexadecimalEscape = hexadecimalEscape,
    Desc.binaryEscape  = Desc.NumericIllegal,
    Desc.decimalEscape = Desc.NumericIllegal,
    Desc.emptyEscape = Nothing,
    Desc.gapsSupported = False
  }

octalEscape :: Desc.NumericEscape
octalEscape = Desc.NumericSupported {
    prefix = Nothing,
    numDigits = Desc.Exactly [3],
    maxValue  = '\999'
  }

hexadecimalEscape :: Desc.NumericEscape
hexadecimalEscape = Desc.NumericSupported {
    prefix = Just 'x',
    numDigits = Desc.Exactly [2],
    maxValue = '\999'
  }
