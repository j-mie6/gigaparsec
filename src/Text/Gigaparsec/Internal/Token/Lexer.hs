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

import Text.Gigaparsec (Parsec, eof, void, empty, (<|>), atomic, unit)
import Text.Gigaparsec.Char (satisfy, string, item, endOfLine)
import Text.Gigaparsec.Combinator (skipMany, skipManyTill)
import Text.Gigaparsec.State (set, get, setDuring, rollback)
import Text.Gigaparsec.Errors.Combinator (hide)

import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Text.Gigaparsec.Token.Errors (
    ErrorConfig (labelSpaceEndOfLineComment, labelSpaceEndOfMultiComment),
    defaultErrorConfig
  )
import Text.Gigaparsec.Internal.Token.Errors (annotate)
import Text.Gigaparsec.Internal.Token.Generic (mkGeneric)
import Text.Gigaparsec.Internal.Token.Symbol (Symbol, mkSym, mkSymbol)
import Text.Gigaparsec.Internal.Token.Symbol qualified as Symbol (lexeme)
import Text.Gigaparsec.Internal.Token.Names (Names, mkNames)
import Text.Gigaparsec.Internal.Token.Names qualified as Names (lexeme)
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

import Text.Gigaparsec.Internal.Require (require)

import Data.List (isPrefixOf)
import Data.IORef (newIORef)
import Data.Ref (fromIORef)
import Control.Exception (Exception, throw)
import Control.Monad (join, guard)
import System.IO.Unsafe (unsafePerformIO)

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

{-|
This type is concerned with special treatment of whitespace.

For the vast majority of cases, the functionality within this object shouldn't be needed, 
as whitespace is consistently handled by lexeme and fully. 
However, for grammars where whitespace is significant (like indentation-sensitive languages), 
this object provides some more fine-grained control over how whitespace is consumed by the parsers within lexeme.
-}
type Space :: *
data Space = Space { 
  {-|
  Skips zero or more (insignificant) whitespace characters as well as comments.

  The implementation of this parser depends on whether 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true: 
  when it is, this parser may change based on the use of the alter combinator. 

  This parser will always use the hide combinator as to not appear as a valid alternative in an error message: 
  it's likely always the case whitespace can be added at any given time, but that doesn't make it a useful suggestion unless it is significant.
  -}
    whiteSpace :: !(Parsec ())
  {-|
  Skips zero or more comments.

  The implementation of this combinator does not vary with 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent'. 
  It will use the hide combinator as to not appear as a valid alternative in an error message: 
  adding a comment is often legal, 
  but not a useful solution for how to make the input syntactically valid.
  -}
  , skipComments :: !(Parsec ())
  {-|
  This combinator changes how lexemes parse whitespace for the duration of a given parser.

  So long as 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true, 
  this combinator will be able to locally change the definition of whitespace during the given parser.

  === __Examples__
  * In indentation sensitive languages, the indentation sensitivity is often ignored within parentheses or braces. 
  In these cases, 
  @parens (alter withNewLine p)@ 
  would allow unrestricted newlines within parentheses.
  -}
  , alter :: forall a. Desc.CharPredicate -> Parsec a -> Parsec a
  {-|
  This parser initialises the whitespace used by the lexer when 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true.

  The whitespace is set to the implementation given by the lexical description.
  This parser must be used, by fully or otherwise, 
  as the first thing the global parser does or an UnfilledRegisterException will occur.

  See 'alter' for how to change whitespace during a parse.
  -}
  , initSpace :: Parsec ()
  }

mkSpace :: Desc.SpaceDesc -> ErrorConfig -> Space
mkSpace desc@Desc.SpaceDesc{..} !errConfig = Space {..}
  where -- don't think we can trust doing initialisation here, it'll happen in some random order
        {-# NOINLINE wsImpl #-}
        !wsImpl = fromIORef (unsafePerformIO (newIORef (error "uninitialised space")))
        comment = commentParser desc -- do not make this strict
        implOf
          | supportsComments desc = hide . maybe skipComments (skipMany . (<|> comment errConfig) . void . satisfy)
          | otherwise             = hide . maybe empty (skipMany . satisfy)
        !configuredWhitespace = implOf space
        !whiteSpace
          | whitespaceIsContextDependent = join (get wsImpl)
          | otherwise                    = configuredWhitespace
        !skipComments = skipMany (comment errConfig)
        alter p
          | whitespaceIsContextDependent = rollback wsImpl . setDuring wsImpl (implOf p)
          | otherwise                    = throw (UnsupportedOperation badAlter)
        initSpace
          | whitespaceIsContextDependent = set wsImpl configuredWhitespace
          | otherwise                    = throw (UnsupportedOperation badInit)
        badInit = "whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is True"
        badAlter = "whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is True"

{-
We have the following invariances to be checked up front:
  * at least one kind of comment must be enabled
  * the starts of line and multiline must not overlap

-- TODO: needs error messages put in (is the hide correct)
-- TODO: remove guard, configure properly
-}
commentParser :: Desc.SpaceDesc -> ErrorConfig -> Parsec ()
commentParser Desc.SpaceDesc{..} !errConfig =
  require (multiEnabled || singleEnabled) "skipComments" noComments $
    require (not (multiEnabled && isPrefixOf multiLineCommentStart lineCommentStart)) "skipComments" noOverlap $
      hide (multiLine <|> singleLine)
  where
    -- can't make these strict until guard is gone
    openComment = atomic (string multiLineCommentStart)
    closeComment = annotate (labelSpaceEndOfMultiComment errConfig) (atomic (string multiLineCommentEnd))
    multiLine = guard multiEnabled *> openComment *> wellNested 1
    wellNested :: Int -> Parsec ()
    wellNested 0 = unit
    wellNested n = closeComment *> wellNested (n - 1)
               <|> guard multiLineNestedComments *> openComment *> wellNested (n + 1)
               <|> item *> wellNested n
    singleLine = guard singleEnabled
              *> atomic (string lineCommentStart)
              *> skipManyTill item (annotate (labelSpaceEndOfLineComment errConfig) endOfLineComment)

    endOfLineComment
      | lineCommentAllowsEOF = void endOfLine <|> eof
      | otherwise            = void endOfLine

    multiEnabled = not (null multiLineCommentStart || null multiLineCommentEnd)
    singleEnabled = not (null lineCommentStart)
    noComments = "one of single- or multi-line comments must be enabled"
    noOverlap = "single-line comments must not overlap with multi-line comments"

supportsComments :: Desc.SpaceDesc -> Bool
supportsComments Desc.SpaceDesc{..} = not (null lineCommentStart && null multiLineCommentStart)

type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation
