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
import Text.Gigaparsec.Registers (put, get, localWith, rollback)
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

import Text.Gigaparsec.Internal.RT (fromIORef)
import Text.Gigaparsec.Internal.Require (require)

import Data.List (isPrefixOf)
import Data.IORef (newIORef)
import Control.Exception (Exception, throw)
import Control.Monad (join, guard)
import System.IO.Unsafe (unsafePerformIO)

type Lexer :: *
data Lexer = Lexer { lexeme :: !Lexeme
                   , nonlexeme :: !Lexeme
                   , fully :: !(forall a. Parsec a -> Parsec a)
                   , space :: !Space
                   }

mkLexer :: Desc.LexicalDesc -> Lexer
mkLexer !desc = mkLexerWithErrorConfig desc defaultErrorConfig

mkLexerWithErrorConfig :: Desc.LexicalDesc -> ErrorConfig -> Lexer
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
type Lexeme :: *
data Lexeme = Lexeme
                { apply :: !(forall a. Parsec a -> Parsec a) -- this is tricky...
                , sym :: !(String -> Parsec ())
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
            | NonLexeme
                { sym :: !(String -> Parsec ())
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

type Space :: *
data Space = Space { whiteSpace :: !(Parsec ())
                   , skipComments :: !(Parsec ())
                   , alter :: forall a. Desc.CharPredicate -> Parsec a -> Parsec a
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
          | whitespaceIsContextDependent = rollback wsImpl . localWith wsImpl (implOf p)
          | otherwise                    = throw (UnsupportedOperation badAlter)
        initSpace
          | whitespaceIsContextDependent = put wsImpl configuredWhitespace
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
