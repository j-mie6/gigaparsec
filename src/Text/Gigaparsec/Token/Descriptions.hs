{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- TODO: In next major, don't expose the constructors of the descriptions,
-- we want them built up by record copy for forwards compatible evolution
-- We can move this into an internal module to accommodate that if we want
{-|
Module      : Text.Gigaparsec.Token.Descriptions
Description : This module contains the descriptions of various lexical structures to configure the lexer.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module contains the descriptions of various lexical structures to configure the lexer.

Many languages share common lexical tokens, such as numeric and string literals.
Writing lexers turning these strings into tokens is effectively boilerplate.
A __Description__ encodes how to lex one of these common tokens.
Feeding a 'LexicalDesc' to a 'Text.Gigaparsec.Token.Lexer.Lexer' provides many combinators
for dealing with these tokens.

==== Usage
Rather than use the internal constructors, such as @NameDesc@, one should extend the \'@plain@\' definitions with record field updates.
For example,

@
myLexicalDesc = plain
  { nameDesc = myNameDesc
  , textDesc = myTextDesc
  }
@

will produce a description that overrides the default name and text descriptions by those given.
See 'plainName', 'plainSymbol', 'plainNumeric', 'plainText' and 'plainSpace' for further examples.

@since 0.2.2.0
-}
module Text.Gigaparsec.Token.Descriptions (
  -- * Lexical Descriptions
  {-|
  A lexer is configured by extending the default 'plain' template, producing a 'LexicalDesc'.

  * 'LexicalDesc'
  * 'plain'

  -}
  -- ** Name Descriptions
  {-|
  A 'NameDesc' configures the lexing of name-like tokens, such as variable and function names.
  To create a 'NameDesc', use 'plainName', and configure it to your liking with record updates.

  * 'NameDesc'

      * 'identifierStart'
      * 'identifierLetter'
      * 'operatorStart'
      * 'operatorLetter'

  * 'plainName'

  -}
  -- ** Symbol Descriptions
  {-|
  A 'SymbolDesc' configures the lexing of \'symbols\' (textual literals), such as keywords and operators.
  To create a 'SymbolDesc', use 'plainSymbol' and configure it to your liking with record updates.

  * 'SymbolDesc'

      * 'hardKeywords'
      * 'hardOperators'
      * 'caseSensitive'

  * 'plainSymbol'

  -}
  -- ** Numeric Descriptions
  {-|
  A 'NumericDesc' configures the lexing of numeric literals, such as integer and floating point literals.
  To create a 'NumericDesc', use 'plainNumeric' and configure it to your liking with record updates.
  Also see 'ExponentDesc', 'BreakCharDesc', and 'PlusSignPresence', for further configuration options.

  * 'NumericDesc'

      * 'literalBreakChar'
      * 'leadingDotAllowed'
      * 'trailingDotAllowed'
      * 'leadingZerosAllowed'
      * 'positiveSign'
      * 'integerNumbersCanBeHexadecimal'
      * 'integerNumbersCanBeOctal'
      * 'integerNumbersCanBeBinary'
      * 'realNumbersCanBeHexadecimal'
      * 'realNumbersCanBeOctal'
      * 'realNumbersCanBeBinary'
      * 'hexadecimalLeads'
      * 'octalLeads'
      * 'binaryLeads'
      * 'decimalExponentDesc'
      * 'hexadecimalExponentDesc'
      * 'octalExponentDesc'
      * 'binaryExponentDesc'

  * 'plainNumeric'
  -}
  -- *** Exponent Descriptions
  {-|
  An 'ExponentDesc' configures scientific exponent notation.

    * 'ExponentDesc'

        * 'NoExponents'
        * 'ExponentsSupported'

            * 'compulsory'
            * 'chars'
            * 'base'
            * 'expSign'
            * 'expLeadingZerosAllowd'
  -}
  -- *** Break-Characters in Numeric Literals
  {-|
  Some languages allow a single numeric literal to be separated by a \'break\' symbol.

    * 'BreakCharDesc'

      * 'NoBreakChar'
      * 'BreakCharSupported'

          * 'breakChar'
          * 'allowedAfterNonDecimalPrefix'
  -}
  -- *** Numeric Literal Prefix Configuration
  {-|

  * 'PlusSignPresence'

      * 'PlusRequired'
      * 'PlusOptional'
      * 'PlusIllegal'
  -}
  -- ** Text Descriptions
  {-|
  A 'TextDesc' configures the lexing of string and character literals, as well as escaped numeric literals.
  To create a 'TextDesc', use 'plainText' and configure it to your liking with record updates.
  See 'EscapeDesc', 'NumericEscape' and 'NumberOfDigits' for further configuration of escape sequences and escaped numeric literals.

  * 'TextDesc'

      * 'escapeSequences'
      * 'characterLiteralEnd'
      * 'stringEnds'
      * 'multiStringEnds'
      * 'graphicCharacter'

  * 'plainText'
  -}
  -- *** Escape Character Descriptions
  {-|
  Configuration of escape sequences, such as tabs @\t@ and newlines @\n@, and
  escaped numbers, such as hexadecimals @0x...@ and binary @0b...@.

  * 'EscapeDesc'

      * 'escBegin'
      * 'literals'
      * 'mapping'
      * 'decimalEscape'
      * 'hexadecimalEscape'
      * 'octalEscape'
      * 'binaryEscape'
      * 'emptyEscape'
      * 'gapsSupported'

  * 'plainEscape'
  -}
  -- *** Numeric Escape Sequences
  {-|
  Configuration of escaped numeric literals.
  For example, hexadecimals, @0x...@.

  * 'NumericEscape'

      * 'NumericIllegal'
      * 'NumericSupported'

          * 'prefix'
          * 'numDigits'
          * 'maxValue'

  * 'NumberOfDigits'

      * 'Unbounded'
      * 'Exactly'
      * 'AtMost'

  -}
  -- ** Whitespace and Comment Descriptions
  {-|
  A 'SpaceDesc' configures the lexing whitespace and comments.
  To create a 'SpaceDesc', use 'plainSpace' and configure it to your liking with record updates.

  * 'SpaceDesc'

      * 'lineCommentStart'
      * 'lineCommentAllowsEOF'
      * 'multiLineCommentStart'
      * 'multiLineCommentEnd'
      * 'multiLineNestedComments'
      * 'space'
      * 'whitespaceIsContextDependent'
      * 'lineContinuationChar'

  * 'plainSpace'
  * 'CharPredicate'

  -}
  module Text.Gigaparsec.Token.Descriptions
  ) where

import Data.Char (isSpace)
import Data.Set (Set)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)

{-|
This type describes the aggregation of a bunch of different sub-configurations for lexing a specific language.

See the 'plain' smart constructor to define a @LexicalDesc@.
-}
type LexicalDesc :: *
data LexicalDesc = LexicalDesc {
  -- | the description of name-like lexemes
     nameDesc :: {-# UNPACK #-} !NameDesc
  -- | the description of specific symbolic lexemes
  , symbolDesc :: {-# UNPACK #-} !SymbolDesc
  -- | the description of numeric literals
  , numericDesc :: {-# UNPACK #-} !NumericDesc
  -- | the description of text literals
  , textDesc :: {-# UNPACK #-} !TextDesc
  -- | the description of whitespace
  , spaceDesc :: {-# UNPACK #-} !SpaceDesc
  }

{-|
This lexical description contains the template @plain\<...\>@ descriptions defined in this module.
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
data NameDesc = NameDesc {
  -- | the characters that start an identifier
    identifierStart :: !CharPredicate
  -- | the characters that continue an identifier
  , identifierLetter :: !CharPredicate
  -- | the characters that start a user-defined operator
  , operatorStart :: !CharPredicate
  -- | the characters that continue a user-defined operator
  , operatorLetter :: !CharPredicate
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
data SymbolDesc = SymbolDesc {
  -- | what keywords are always treated as keywords within the language.
    hardKeywords :: !(Set String)
  -- | what operators are always treated as reserved operators within the language.
  , hardOperators :: !(Set String)
  -- | @True@ if the keywords are case sensitive, @False@ if not (so that e.g. @IF = if@).
  , caseSensitive :: !Bool
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
data NumericDesc = NumericDesc {
  -- | can breaks be found within numeric literals? (see 'BreakCharDesc')
    literalBreakChar :: !BreakCharDesc
  -- | can a real number omit a leading 0 before the point?
  , leadingDotAllowed :: !Bool
  -- | can a real number omit a trailing 0 after the point?
  , trailingDotAllowed :: !Bool
  -- | are extraneous zeros allowed at the start of decimal numbers?
  , leadingZerosAllowed :: !Bool
  -- | describes if positive (+) signs are allowed, compulsory, or illegal.
  , positiveSign :: !PlusSignPresence
  -- generic number
  -- | can generic "integer numbers" to be hexadecimal?
  , integerNumbersCanBeHexadecimal :: !Bool
  -- | can generic "integer numbers" to be octal?
  , integerNumbersCanBeOctal :: !Bool
  -- | can generic "integer numbers" to be binary?
  , integerNumbersCanBeBinary :: !Bool
  -- | can generic "real numbers" to be hexadecimal?
  , realNumbersCanBeHexadecimal :: !Bool
  -- | can generic "real numbers" to be octal?
  , realNumbersCanBeOctal :: !Bool
  -- | can generic "real numbers" to be binary?
  , realNumbersCanBeBinary :: !Bool
  -- special literals
  -- | the characters that begin a hexadecimal literal following a 0 (may be empty).
  , hexadecimalLeads :: !(Set Char)
  -- | the characters that begin an octal literal following a 0 (may be empty).
  , octalLeads :: !(Set Char)
  -- | the characters that begin a binary literal following a 0 (may be empty).
  , binaryLeads :: !(Set Char)

  -- exponents
  -- | describes how scientific exponent notation should work for decimal literals.
  , decimalExponentDesc :: !ExponentDesc
  -- | describes how scientific exponent notation should work for hexadecimal literals.
  , hexadecimalExponentDesc :: !ExponentDesc
  -- | describes how scientific exponent notation should work for octal literals.
  , octalExponentDesc :: !ExponentDesc
  -- | describes how scientific exponent notation should work for binary literals.
  , binaryExponentDesc :: !ExponentDesc
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
  , chars = [\'e\']  -- The letter \'e\' separates the significand from the exponent
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

{-|
  This type describes how to parse string and character literals.
-}
type TextDesc :: *
data TextDesc = TextDesc
  { escapeSequences :: {-# UNPACK #-} !EscapeDesc -- ^ the description of escape sequences in literals.
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
  , literals :: !(Set Char)             -- ^ the characters that can be directly escaped, but still represent themselves, for instance \'"\', or \'\\\'.
  , mapping :: !(Map String Char)       -- ^ the possible escape sequences that map to a character other than themselves and the (full UTF-16) character they map to, for instance "n" -> 0xa
  , decimalEscape :: !NumericEscape     -- ^ if allowed, the description of how numeric escape sequences work for base 10.
  , hexadecimalEscape :: !NumericEscape -- ^ if allowed, the description of how numeric escape sequences work for base 16
  , octalEscape :: !NumericEscape       -- ^ if allowed, the description of how numeric escape sequences work for base 8
  , binaryEscape :: !NumericEscape      -- ^ if allowed, the description of how numeric escape sequences work for base 2
  , emptyEscape :: !(Maybe Char)        -- ^ if one should exist, the character which has no effect on
                                        -- the string but can be used to disambiguate other escape sequences: in Haskell this would be \&
  , gapsSupported :: !Bool              -- ^ specifies whether or not string gaps are supported:
                                        -- this is where whitespace can be injected between two escBegin characters and this will all be ignored in the final string,
                                        -- such that @"hello \ \world"@ is "hello world"
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
data SpaceDesc = SpaceDesc
  { lineCommentStart :: !String           -- ^ how to start single-line comments (empty for no single-line comments).
  , lineCommentAllowsEOF :: !Bool         -- ^ can a single-line comment be terminated by the end-of-file (@True@), or must it end with a newline (@False@)?
  , multiLineCommentStart :: !String      -- ^ how to start multi-line comments (empty for no multi-line comments).
  , multiLineCommentEnd :: !String        -- ^ how to end multi-line comments (empty for no multi-line comments).
  , multiLineNestedComments :: !Bool      -- ^ @True@ when multi-line comments can be nested, @False@ otherwise.
  , space :: !CharPredicate               -- ^ the characters to be treated as whitespace
  , whitespaceIsContextDependent :: !Bool -- ^ does the context change the definition of whitespace (@True@), or not (@False@)?
                                          --  (e.g. in Python, newlines are valid whitespace within parentheses, but are significant outside of them)
  , lineContinuationChar :: !(Maybe Char) -- ^ A character that invokes line continuations.
                                          -- For example, '\' in Python allows the user to split an expression over the next line, ignoring indentation.
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
                       , lineContinuationChar = Nothing
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

amendCharPredicate :: Char -> CharPredicate -> CharPredicate
amendCharPredicate c Nothing = Just (\x -> c == x)
amendCharPredicate c (Just f) = if f c then Just f else Just (\x -> c == x || f x)


