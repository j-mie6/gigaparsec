{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- TODO: In next major, don't expose the constructors of the descriptions,
-- we want them built up by record copy for forwards compatible evolution
-- We can move this into an internal module to accommodate that if we want
  {-|
Module      : Text.Gigaparsec.Token.Descriptions
Description : This module contains the descriptions of various lexical structures to be fed to the lexer.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module contains the descriptions of various lexical structures to be fed to the lexer.

Rather than use the internal constructors, such as @NameDesc@, one should extend the \'@plain@\' definitions with record field updates.
For example,

@
  myLexicalDesc = plain 
    { nameDesc = myNameDesc,
      textDesc = myTextDesc  
    }
@

will produce a description that overrides the default name and text descriptions by those given.
See 'plainName', 'plainSymbol', 'plainNumeric', 'plainText' and 'plainSpace' for further examples.

@since 0.2.2.0
-}
module Text.Gigaparsec.Token.Descriptions (module Text.Gigaparsec.Token.Descriptions) where

import Data.Char (isSpace)
import Data.Set (Set)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)

{-|
This type describes the aggregation of a bunch of different sub-configurations for lexing a specific language.

See the 'plain' smart constructor to define a @LexicalDesc@.
-}
type LexicalDesc :: *
data LexicalDesc = LexicalDesc 
  { nameDesc :: {-# UNPACK #-} !NameDesc       -- ^ the description of name-like lexemes
  , symbolDesc :: {-# UNPACK #-} !SymbolDesc   -- ^ the description of specific symbolic lexemes
  , numericDesc :: {-# UNPACK #-} !NumericDesc -- ^ the description of numeric literals
  , textDesc :: {-# UNPACK #-} !TextDesc       -- ^ the description of text literals
  , spaceDesc :: {-# UNPACK #-} !SpaceDesc     -- ^ the description of whitespace
  }

{-|
This lexical description contains the template @plain<...>@ descriptions defined in this module.
See 'plainName', 'plainSymbol', 'plainNumeric', 'plainText' and 'plainSpace' for how this description configures the lexer.
-}
plain :: LexicalDesc
plain = LexicalDesc { nameDesc = plainName
                    , symbolDesc = plainSymbol
                    , numericDesc = plainNumeric
                    , textDesc = plainText
                    , spaceDesc = plainSpace
                    }

{-|
This type describes how name-like things are described lexically.

In particular, this defines which characters will constitute identifiers and operators.

See the 'plainName' smart constructor for how to implement a custom name description.
-}
type NameDesc :: *
data NameDesc = NameDesc 
  { identifierStart :: !CharPredicate  -- ^ the characters that start an identifier
  , identifierLetter :: !CharPredicate -- ^ the characters that continue an identifier
  , operatorStart :: !CharPredicate    -- ^ the characters that start a user-defined operator
  , operatorLetter :: !CharPredicate   -- ^ the characters that continue a user-defined operator
  }

{-|
This is a blank name description template, which should be extended to form a custom name description.

In its default state, 'plainName' makes no characters able to be part of an identifier or operator.
To change this, one should use record field copies, for example:

@
myNameDesc :: NameDesc
myNameDesc = plainName 
  { identifierStart = myIdentifierStartPredicate 
  , identifierLetter = myIdentifierLetterPredicate
  }
@

@myNameDesc@ with then lex identifiers according to the given predicates.

-}
plainName :: NameDesc
plainName = NameDesc { identifierStart = Nothing
                     , identifierLetter = Nothing
                     , operatorStart = Nothing
                     , operatorLetter = Nothing
                     }


{-|
This type describes how symbols (textual literals in a BNF) should be processed lexically, including keywords and operators.

This includes keywords and (hard) operators that are reserved by the language. 
For example, in Haskell, "data" is a keyword, and "->" is a hard operator.

See the 'plainSymbol' smart constructor for how to implement a custom name description.
-}
type SymbolDesc :: *
data SymbolDesc = SymbolDesc 
  { hardKeywords :: !(Set String)  -- ^ what keywords are always treated as keywords within the language.
  , hardOperators :: !(Set String) -- ^ what operators are always treated as reserved operators within the language.
  , caseSensitive :: !Bool         -- ^ @True@ if the keywords are case sensitive, @False@ if not (so that e.g. @IF = if@).
  }

{-|
This is a blank symbol description template, which should be extended to form a custom symbol description.

In its default state, 'plainSymbol' has no keywords or reserved/hard operators.
To change this, one should use record field copies, for example:

@
{-# LANGUAGE OverloadedLists #-} -- This lets us write @[a,b]@ to get a 'Data.Set' containing @a@ and @b@
                                 -- If you don't want to use this, just use @'Data.Set.fromList' [a,b]@
mySymbolDesc :: SymbolDesc
mySymbolDesc = plainSymbol 
  { hardKeywords = ["data", "where"] 
  , hardOperators = ["->"]
  , caseSensitive = True
  }
@

@mySymbolDesc@ with then treat @data@ and @where@ as keywords, and @->@ as a reserved operator.

-}
plainSymbol :: SymbolDesc
plainSymbol = SymbolDesc { hardKeywords = []
                         , hardOperators = []
                         , caseSensitive = True
                         }

{-|
This type describes how numeric literals (integers, decimals, hexadecimals, etc...), should be lexically processed.
-}
type NumericDesc :: *
data NumericDesc = NumericDesc 
  { literalBreakChar :: !BreakCharDesc        -- ^ can breaks be found within numeric literals? (see 'BreakCharDesc')
  , leadingDotAllowed :: !Bool                -- ^ can a real number omit a leading 0 before the point?
  , trailingDotAllowed :: !Bool               -- ^ can a real number omit a trailing 0 after the point?
  , leadingZerosAllowed :: !Bool              -- ^ are extraneous zeros allowed at the start of decimal numbers?
  , positiveSign :: !PlusSignPresence         -- ^ describes if positive (+) signs are allowed, compulsory, or illegal.
  -- generic number
  , integerNumbersCanBeHexadecimal :: !Bool   -- ^ can generic "integer numbers" to be hexadecimal?
  , integerNumbersCanBeOctal :: !Bool         -- ^ can generic "integer numbers" to be octal?
  , integerNumbersCanBeBinary :: !Bool        -- ^ can generic "integer numbers" to be binary?
  , realNumbersCanBeHexadecimal :: !Bool      -- ^ can generic "real numbers" to be hexadecimal?
  , realNumbersCanBeOctal :: !Bool            -- ^ can generic "real numbers" to be octal?
  , realNumbersCanBeBinary :: !Bool           -- ^ can generic "real numbers" to be binary?
  -- special literals
  , hexadecimalLeads :: !(Set Char)           -- ^ the characters that begin a hexadecimal literal following a 0 (may be empty).
  , octalLeads :: !(Set Char)                 -- ^ the characters that begin an octal literal following a 0 (may be empty).
  , binaryLeads :: !(Set Char)                -- ^ the characters that begin a binary literal following a 0 (may be empty).
  -- exponents
  , decimalExponentDesc :: !ExponentDesc      -- ^ describes how scientific exponent notation should work for decimal literals.
  , hexadecimalExponentDesc :: !ExponentDesc  -- ^ describes how scientific exponent notation should work for hexadecimal literals.
  , octalExponentDesc :: !ExponentDesc        -- ^ describes how scientific exponent notation should work for octal literals.
  , binaryExponentDesc :: !ExponentDesc       -- ^ describes how scientific exponent notation should work for binary literals.
  }

{-|
This is a blank numeric description template, which should be extended to form a custom numeric description.

In its default state, 'plainNumeric' allows for hex-, oct-, and bin-ary numeric literals,
with the standard prefixes.
To change this, one should use record field copies.
-}
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

{-|
Describe how scientific exponent notation can be used within real literals.

A common notation would be @1.6e3@ for @1.6 × 10³@, which the following @ExponentDesc@ describes:

@
{-# LANGUAGE OverloadedLists #-} -- Lets us write @[a]@ to generate a singleton 'Data.Set' containing @a@.
usualNotation :: ExponentDesc
usualNotation = ExponentsSupported 
  { compulsory = False
  , chars = [\'e\']  -- The letter 'e' separates the significand from the exponent
  , base  = 10   -- The base of the exponent is 10, so that @2.3e5@ means @2.3 × 10⁵@
  , expSign = PlusOptional -- A positive exponent does not need a plus sign, but can have one.
  , expLeadingZerosAllowd = True -- We allow leading zeros on exponents; so @1.2e005@ is valid.
  }
@

-}
type ExponentDesc :: *
data ExponentDesc 
  = NoExponents         -- ^ The language does not allow exponent notation.
  | ExponentsSupported  -- ^ The language does allow exponent notation, according to the following fields:
    { compulsory :: !Bool            -- ^ Is exponent notation required for real literals?
    , chars :: !(Set Char)           -- ^ The characters that separate the significand from the exponent
    , base :: !Int                   -- ^ The base of the exponent; this is usually base ten.
    , expSign :: !PlusSignPresence   -- ^ Is a plus (@+@) sign required for positive exponents?
    , expLeadingZerosAllowd :: !Bool -- ^ Can the exponent contain leading zeros; for example is @3.2e005@ valid?
    }

{-|
Prescribes whether or not numeric literals can be broken up by a specific symbol.

For example, can one write @300.2_3@?
-}
type BreakCharDesc :: *
data BreakCharDesc 
  = NoBreakChar                             -- ^ Literals cannot be broken.
  | BreakCharSupported                      -- ^ Literals can be broken.
    { breakChar :: !Char                    -- ^ the character allowed to break a literal (often _).
    , allowedAfterNonDecimalPrefix :: !Bool -- ^ can non-decimals be broken; e.g. can one write, 0x_300?
    }

{-|
Whether or not a plus sign (@+@) can prefix a numeric literal.
-}
type PlusSignPresence :: *
data PlusSignPresence 
  = PlusRequired -- ^ (@+@) must always precede a positive numeric literal
  | PlusOptional -- ^ (@+@) may precede a positive numeric literal, but is not necessary
  | PlusIllegal  -- ^ (@+@) cannot precede a numeric literal as a prefix (this is separate to allowing an infix binary @+@ operator).

-- TODO: this should be moved into the exports of the module once we update the API.
{-|
=== Text Descriptions

Most languages must be able to parse strings and characters.
'TextDesc' and 'plainText' describe how to parse these.

-}

{-|
  This type describes how to parse string and character literals.
-}
type TextDesc :: *
data TextDesc = TextDesc 
  { escapeSequences :: {-# UNPACK #-} !EscapeDesc -- ^ the description of how escape sequences in literals.
  , characterLiteralEnd :: !Char -- ^ the character that starts and ends a character literal.
  , stringEnds :: !(Set (String, String)) -- ^ the sequences that may begin and end a string literal.
  , multiStringEnds :: !(Set (String, String)) -- ^ the sequences that may begin and end a multi-line string literal.
  , graphicCharacter :: !CharPredicate -- ^ the characters that can be written verbatim into a character or string literal.
  }

{-|
This is a blank text description template, which should be extended to form a custom text description.

In its default state, 'plainText' parses characters as symbols between @\'@ and @\'@, and strings between @"@ and @"@.
To change this, one should use record field copies, for example:

@

{-# LANGUAGE OverloadedLists #-} -- This lets us write @[a,b]@ to get a 'Data.Set' containing @a@ and @b@
                                 -- If you don't want to use this, just use @'Data.Set.fromList' [a,b]@
myPlainText:: TextDesc
myPlainText= plainText 
  { characterLiteralEnd = a
  , stringEnds = [(b, c)]
  }

@

@myPlainText@ with then parse characters as a single character between @a@ and @a@, and a string as characters between @b@ and @c@.
-}
plainText :: TextDesc
plainText = TextDesc { escapeSequences = plainEscape
                     , characterLiteralEnd = '\''
                     , stringEnds = [("\"", "\"")]
                     , multiStringEnds = []
                     , graphicCharacter = Just (>= ' ')
                     }

{-|
Defines the escape characters, and their meaning.

This includes character escapes (e.g. tabs, carriage returns), and numeric escapes, such as binary (usually \"0b\") and hexadecimal, \"0x\".
-}
type EscapeDesc :: *
data EscapeDesc = EscapeDesc 
  { escBegin :: !Char                   -- ^ the character that begins an escape sequence: this is usually @\\@.
  , literals :: !(Set Char)             -- ^ the characters that can be directly escaped, but still represent themselves, for instance '"', or '\\'.
  , mapping :: !(Map String Char)       -- ^ the possible escape sequences that map to a character other than themselves and the (full UTF-16) character they map to, for instance "n" -> 0xa
  , decimalEscape :: !NumericEscape     -- ^ if allowed, the description of how numeric escape sequences work for base 10.
  , hexadecimalEscape :: !NumericEscape -- ^ if allowed, the description of how numeric escape sequences work for base 16
  , octalEscape :: !NumericEscape       -- ^ if allowed, the description of how numeric escape sequences work for base 8
  , binaryEscape :: !NumericEscape      -- ^ if allowed, the description of how numeric escape sequences work for base 2
  , emptyEscape :: !(Maybe Char)        -- ^ if one should exist, the character which has no effect on 
                                        -- the string but can be used to disambiguate other escape sequences: in Haskell this would be \&
  , gapsSupported :: !Bool              -- ^ specifies whether or not string gaps are supported: 
                                        -- this is where whitespace can be injected between two escBegin characters and this will all be ignored in the final string, 
                                        -- such that "hello \ \world" is "hello world"
  }

{-|
This is a blank escape description template, which should be extended to form a custom escape description.

In its default state, 'plainEscape' the only escape symbol is a backslash, \"\\\\".
To change this, one should use record field copies, for example:

@

{-# LANGUAGE OverloadedLists #-} -- This lets us write @[a,b]@ to get a 'Data.Set' containing @a@ and @b@,
                                 -- and [(a,b),(c,d)] for a 'Data.Map' which sends @a ↦ b@ and @c ↦ d@
myPlainEscape:: EscapeDesc
myPlainEscape= plainEscape 
  { literals = a
  , stringEnds = [(b, c)]
  , mapping = [("t",0x0009), ("r",0x000D)]
  , hexadecimalEscape = NumericSupported TODO
  }

@

@myPlainText@ with then parse characters as a single character between @a@ and @a@, and a string as characters between @b@ and @c@.
-}
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

{-|
Describes how numeric escape sequences should work for a given base.
-}
type NumericEscape :: *
data NumericEscape 
  = NumericIllegal    -- ^ Numeric literals are disallowed for this specific base.
  | NumericSupported  -- ^ Numeric literals are supported for this specific base.
    { prefix :: !(Maybe Char)      -- ^ the character, if any, that is required to start the literal (like x for hexadecimal escapes in some languages).
    , numDigits :: !NumberOfDigits -- ^ the number of digits required for this literal: this may be unbounded, an exact number, or up to a specific number.
    , maxValue :: !Char            -- ^ the largest character value that can be expressed by this numeric escape.
    }

{-|
Describes how many digits a numeric escape sequence is allowed.
-}
type NumberOfDigits :: *
data NumberOfDigits 
  = Unbounded -- ^ there is no limit on the number of digits that may appear in this sequence.
  | Exactly !(NonEmpty Word) -- ^ the number of digits in the literal must be one of the given values.
  | AtMost -- ^ there must be at most @n@ digits in the numeric escape literal, up to and including the value given. 
    !Word  -- ^ the maximum (inclusive) number of digits allowed in the literal..

{-|
This type describes how whitespace and comments should be handled lexically.
-}
type SpaceDesc :: *
data SpaceDesc = SpaceDesc { lineCommentStart :: !String           -- ^ how to start single-line comments (empty for no single-line comments).
                           , lineCommentAllowsEOF :: !Bool         -- ^ can a single-line comment be terminated by the end-of-file (@True@), or must it end with a newline (@False@)?
                           , multiLineCommentStart :: !String      -- ^ how to start multi-line comments (empty for no multi-line comments).
                           , multiLineCommentEnd :: !String        -- ^ how to end multi-line comments (empty for no multi-line comments).
                           , multiLineNestedComments :: !Bool      -- ^ @True@ when multi-line comments can be nested, @False@ otherwise.
                           , space :: !CharPredicate               -- ^ the characters to be treated as whitespace
                           , whitespaceIsContextDependent :: !Bool -- ^ does the context change the definition of whitespace (@True@), or not (@False@)? 
                                                                   --  (e.g. in Python, newlines are valid whitespace within parentheses, but are significant outside of them)
                           }
{-|
This is a blank whitespace description template, which should be extended to form the desired whitespace descriptions.

In its default state, 'plainName' makes no comments possible, and the only whitespace characters are those
defined by 'GHC.Unicode.isSpace'
-}
plainSpace :: SpaceDesc
plainSpace = SpaceDesc { lineCommentStart = ""
                       , lineCommentAllowsEOF = True
                       , multiLineCommentStart = ""
                       , multiLineCommentEnd = ""
                       , multiLineNestedComments = False
                       , space = Just isSpace
                       , whitespaceIsContextDependent = False
                       }

{-|
An optional predicate on characters: 
if @pred :: CharPredicate@ and @pred x = Just True@, then the lexer should accept the character @x@.

==== __Examples__
- A predicate that only accepts alphabetical or numbers:
  
  @
    isAlphaNumPred = Just . isAlphaNum 
  @

- A predicate that only accepts capital letters:
  
  @
    isCapital = Just . isAsciiUpper
  @

-}
type CharPredicate :: *
type CharPredicate = Maybe (Char -> Bool)
