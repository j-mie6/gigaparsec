{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.Lexer (
    Lexer, mkLexer, mkLexerWithErrorConfig,
    Lexeme, lexeme, nonlexeme, fully, space,
    apply, sym, symbol, names, -- more go here, no numeric and no text
    -- Numeric
    integer, natural,
    -- Text
    stringLiteral, rawStringLiteral, multiStringLiteral, rawMultiStringLiteral,
    charLiteral,
    -- Space
    Space, skipComments, whiteSpace, alter, initSpace,
  ) where

import Text.Gigaparsec (Parsec, eof)

import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Text.Gigaparsec.Token.Errors (ErrorConfig, defaultErrorConfig)
import Text.Gigaparsec.Internal.Token.Generic (mkGeneric)
import Text.Gigaparsec.Internal.Token.Symbol (Symbol, mkSym, mkSymbol)
import Text.Gigaparsec.Internal.Token.Symbol qualified as Symbol (lexeme)
import Text.Gigaparsec.Internal.Token.Names (Names, mkNames)
import Text.Gigaparsec.Internal.Token.Names qualified as Names (lexeme)
import Text.Gigaparsec.Internal.Token.Space (Space, whiteSpace, skipComments, alter, initSpace, mkSpace)
import Text.Gigaparsec.Internal.Token.Numeric (
    IntegerParsers, mkSigned, mkUnsigned,
    --FloatingParsers, mkSignedFloating, mkUnsignedFloating,
    --CombinedParsers, mkSignedCombined, mkUnsignedCombined,
  )
import Text.Gigaparsec.Internal.Token.BitBounds (CanHoldSigned, CanHoldUnsigned)
import Text.Gigaparsec.Internal.Token.Numeric qualified as Numeric (lexemeInteger, {-lexemeFloating, lexemeCombined-})
import Text.Gigaparsec.Internal.Token.Text (
    TextParsers,
    mkStringParsers, mkCharacterParsers, mkEscape, mkEscapeChar, StringChar(RawChar)
  )
import Text.Gigaparsec.Internal.Token.Text qualified as Text (lexeme)



{-|
A lexer describes how to transform the input string into a series of tokens.
-}
type Lexer :: *
data Lexer = Lexer {
  -- | This contains parsers for tokens treated as "words", 
  -- such that whitespace will be consumed after each token has been parsed.
    lexeme :: !Lexeme
  -- | This contains parsers for tokens that do not give any special treatment to whitespace.
  , nonlexeme :: !Lexeme
  -- | This combinator ensures a parser fully parses all available input, and consumes whitespace at the start.
  , fully :: !(forall a. Parsec a -> Parsec a)
  -- | This contains parsers that directly treat whitespace.
  , space :: !Space
  }

{-|
Create a 'Lexer' with a given description for the lexical structure of the language.
-}
mkLexer :: Desc.LexicalDesc -- ^ The description of the lexical structure of the language.
        -> Lexer -- ^ A lexer which can convert the input stream into a series of lexemes.
mkLexer !desc = mkLexerWithErrorConfig desc defaultErrorConfig

{-|
Create a 'Lexer' with a given description for the lexical structure of the language, 
which reports errors according to the given error config.
-}
mkLexerWithErrorConfig  :: Desc.LexicalDesc -- ^ The description of the lexical structure of the language.
                        -> ErrorConfig -- ^ The description of how to process errors during lexing.
                        -> Lexer -- ^ A lexer which can convert the input stream into a series of lexemes.
mkLexerWithErrorConfig Desc.LexicalDesc{..} !errConfig = Lexer {..}
  where apply p = p <* whiteSpace space
        gen = mkGeneric errConfig
        -- DO NOT HAVE MUTUALLY RECURSIVE FIELDS
        lexeme = Lexeme { apply = apply
                        , sym = apply . sym nonlexeme
                        , symbol = Symbol.lexeme apply symbolNonLexeme
                        , names = Names.lexeme apply (names nonlexeme)
                        , natural = Numeric.lexemeInteger apply naturalNonLexeme
                        , integer = Numeric.lexemeInteger apply (integer nonlexeme)
                        {-, floating = Numeric.lexemeFloating apply (floating nonlexeme)
                        , unsignedCombined =
                            Numeric.lexemeCombined apply (unsignedCombined nonlexeme)
                        , signedCombined =
                            Numeric.lexemeCombined apply (signedCombined nonlexeme)-}
                        , stringLiteral = Text.lexeme apply (stringLiteral nonlexeme)
                        , rawStringLiteral = Text.lexeme apply (rawStringLiteral nonlexeme)
                        , multiStringLiteral = Text.lexeme apply (multiStringLiteral nonlexeme)
                        , rawMultiStringLiteral = Text.lexeme apply (rawMultiStringLiteral nonlexeme)
                        , charLiteral = Text.lexeme apply (charLiteral nonlexeme)
                        }
        nonlexeme = NonLexeme { sym = mkSym symbolDesc symbolNonLexeme errConfig
                              , symbol = symbolNonLexeme
                              , names = mkNames nameDesc symbolDesc errConfig
                              , natural = naturalNonLexeme
                              , integer = mkSigned numericDesc naturalNonLexeme errConfig
                              {-, floating = mkSignedFloating numericDesc positiveFloating
                              , unsignedCombined = mkUnsignedCombined numericDesc naturalNonLexeme positiveFloating
                              , signedCombined = mkSignedCombined numericDesc (unsignedCombined nonlexeme)-}
                              , stringLiteral = mkStringParsers stringEnds escapeChar graphicCharacter False errConfig
                              , rawStringLiteral = mkStringParsers stringEnds rawChar graphicCharacter False errConfig
                              , multiStringLiteral = mkStringParsers multiStringEnds escapeChar graphicCharacter True errConfig
                              , rawMultiStringLiteral = mkStringParsers multiStringEnds rawChar graphicCharacter True errConfig
                              , charLiteral = mkCharacterParsers textDesc escape errConfig
                              }
        !symbolNonLexeme = mkSymbol symbolDesc nameDesc errConfig
        !naturalNonLexeme = mkUnsigned numericDesc gen errConfig
        --positiveFloating = mkUnsignedFloating numericDesc naturalNonLexeme gen
        !escape = mkEscape (Desc.escapeSequences textDesc) gen errConfig
        graphicCharacter = Desc.graphicCharacter textDesc
        stringEnds = Desc.stringEnds textDesc
        multiStringEnds = Desc.multiStringEnds textDesc
        rawChar = RawChar
        escapeChar = mkEscapeChar (Desc.escapeSequences textDesc) escape (whiteSpace space) errConfig
        fully' p = whiteSpace space *> p <* eof
        fully p
          | Desc.whitespaceIsContextDependent spaceDesc = initSpace space *> fully' p
          | otherwise                                   = fully' p
        space = mkSpace spaceDesc errConfig

--TODO: better name for this, I guess?
{-|
A 'Lexeme' is a collection of parsers for handling various tokens (such as symbols and names), where either all or none of the parsers consume whitespace.
-}
type Lexeme :: *
data Lexeme =
  -- | The parsers do consume whitespace
  Lexeme {
      -- | This turns a non-lexeme parser into a lexeme one by ensuring whitespace is consumed after the parser.
        apply :: !(forall a. Parsec a -> Parsec a) -- this is tricky...
      -- | Parse the given string.
      , sym :: !(String -> Parsec ())
      -- | This contains lexing functionality relevant to the parsing of atomic symbols.
      , symbol :: !Symbol
      -- | This contains lexing functionality relevant to the parsing of names, which include operators or identifiers.
      -- The parsing of names is mostly concerned with finding the longest valid name that is not a reserved name, 
      -- such as a hard keyword or a special operator.
      , names :: !Names
      -- | A collection of parsers concerned with handling unsigned (positive) integer literals.
      , natural :: !(IntegerParsers CanHoldUnsigned)
      {-|
      This is a collection of parsers concerned with handling signed integer literals.

      Signed integer literals are an extension of unsigned integer literals which may be prefixed by a sign.
      -}
      , integer :: !(IntegerParsers CanHoldSigned)
      -- desperate times, desperate measures
      --, floating :: !FloatingParsers
      --, unsignedCombined :: !CombinedParsers
      --, signedCombined :: !CombinedParsers
      {-|
      A collection of parsers concerned with handling single-line string literals.

      String literals are generally described by the 'Text.Gigaparsec.Token.Descriptions.TextDesc' fields:

      * 'Text.Gigaparsec.Token.Descriptions.stringEnds'
      * 'Text.Gigaparsec.Token.Descriptions.graphicCharacter'
      * 'Text.Gigaparsec.Token.Descriptions.escapeSequences'

      -}
      , stringLiteral :: !(TextParsers String)
      {-|
      A collection of parsers concerned with handling single-line string literals, /without/ handling any escape sequences:
      this includes literal-end characters and the escape prefix (often @"@ and @\\@ respectively).

      String literals are generally described by the 'Text.Gigaparsec.Token.Descriptions.TextDesc' fields:

      * 'Text.Gigaparsec.Token.Descriptions.stringEnds'
      * 'Text.Gigaparsec.Token.Descriptions.graphicCharacter'
      * 'Text.Gigaparsec.Token.Descriptions.escapeSequences'

      -}
      , rawStringLiteral :: !(TextParsers String)
      {-|
      A collection of parsers concerned with handling multi-line string literals.

      Multi-string literals are generally described by the 'Text.Gigaparsec.Token.Descriptions.TextDesc' fields:

      * 'Text.Gigaparsec.Token.Descriptions.multiStringEnds'
      * 'Text.Gigaparsec.Token.Descriptions.graphicCharacter'
      * 'Text.Gigaparsec.Token.Descriptions.escapeSequences'

      -}
      , multiStringLiteral :: !(TextParsers String)
      {-|
      A collection of parsers concerned with handling multi-line string literals, /without/ handling any escape sequences:
      this includes literal-end characters and the escape prefix (often @"@ and @\\@ respectively).

      Multi-string literals are generally described by the 'Text.Gigaparsec.Token.Descriptions.TextDesc' fields:

      * 'Text.Gigaparsec.Token.Descriptions.multiStringEnds'
      * 'Text.Gigaparsec.Token.Descriptions.graphicCharacter'
      * 'Text.Gigaparsec.Token.Descriptions.escapeSequences'
  
      -}
      , rawMultiStringLiteral :: !(TextParsers String)
      {-|
      A collection of parsers concerned with handling character literals.

      Charcter literals are generally described by the 'Text.Gigaparsec.Token.Descriptions.TextDesc' fields:

      * 'Text.Gigaparsec.Token.Descriptions.characterLiteralEnd'
      * 'Text.Gigaparsec.Token.Descriptions.graphicCharacter'
      * 'Text.Gigaparsec.Token.Descriptions.escapeSequences'

      -}
      , charLiteral :: !(TextParsers Char)
      }
  -- | The parsers do not consume whitespace
  | NonLexeme {
        sym :: !(String -> Parsec ())
      , symbol :: !Symbol
      , names :: !Names
      , natural :: !(IntegerParsers CanHoldUnsigned)
      , integer :: !(IntegerParsers CanHoldSigned)
      -- desperate times, desperate measures
      --, floating :: !FloatingParsers
      --, unsignedCombined :: !CombinedParsers
      --, signedCombined :: !CombinedParsers
      , stringLiteral :: !(TextParsers String)
      , rawStringLiteral :: !(TextParsers String)
      , multiStringLiteral :: !(TextParsers String)
      , rawMultiStringLiteral :: !(TextParsers String)
      , charLiteral :: !(TextParsers Char)
      }
