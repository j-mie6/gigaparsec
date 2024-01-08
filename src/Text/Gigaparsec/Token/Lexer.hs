{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- Ideally, we should probably expose all the functionally via this one file
-- for ergonomics
module Text.Gigaparsec.Token.Lexer (
    Lexer, mkLexer,
    lexeme, nonlexeme, fully, space,
    apply, sym, symbol, names, -- more go here, no numeric and no text
    -- Symbol
    softKeyword, softOperator,
    -- Names
    identifier, identifier', userDefinedOperator, userDefinedOperator',
    -- Numeric
    -- Text
    -- Space
    skipComments, whiteSpace, alter, initSpace,
  ) where

import Text.Gigaparsec (Parsec, eof, void, empty, (<|>), atomic, unit)
import Text.Gigaparsec.Char (satisfy, string, item, endOfLine)
import Text.Gigaparsec.Combinator (skipMany, skipManyTill)
import Text.Gigaparsec.Registers (put, get, localWith, rollback)
import Text.Gigaparsec.Errors.Combinator (hide)

import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Text.Gigaparsec.Token.Generic (mkGeneric)
import Text.Gigaparsec.Token.Symbol (Symbol, mkSym, mkSymbol, softKeyword, softOperator)
import Text.Gigaparsec.Token.Symbol qualified as Symbol (lexeme)
import Text.Gigaparsec.Token.Names (
    Names, mkNames, identifier, identifier', userDefinedOperator, userDefinedOperator'
  )
import Text.Gigaparsec.Token.Names qualified as Names (lexeme)
import Text.Gigaparsec.Token.Numeric (
    IntegerParsers, mkSigned, mkUnsigned,
    FloatingParsers, mkSignedFloating, mkUnsignedFloating,
    CombinedParsers, mkSignedCombined, mkUnsignedCombined, CanHoldSigned, CanHoldUnsigned
  )
import Text.Gigaparsec.Token.Numeric qualified as Numeric (lexemeInteger, lexemeFloating, lexemeCombined)

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
mkLexer Desc.LexicalDesc{..} = Lexer {..}
  where apply p = p <* whiteSpace space
        gen = mkGeneric
        lexeme = Lexeme { apply = apply
                        , sym = apply . sym nonlexeme
                        , symbol = Symbol.lexeme apply (symbol nonlexeme)
                        , names = Names.lexeme apply (names nonlexeme)
                        , natural = Numeric.lexemeInteger apply (natural nonlexeme)
                        , integer = Numeric.lexemeInteger apply (integer nonlexeme)
                        , floating = Numeric.lexemeFloating apply (floating nonlexeme)
                        , unsignedCombined =
                            Numeric.lexemeCombined apply (unsignedCombined nonlexeme)
                        , signedCombined =
                            Numeric.lexemeCombined apply (signedCombined nonlexeme)
                        }
        nonlexeme = NonLexeme { sym = mkSym symbolDesc (symbol nonlexeme)
                              , symbol = mkSymbol symbolDesc nameDesc
                              , names = mkNames nameDesc symbolDesc
                              , natural = mkUnsigned numericDesc gen
                              , integer = mkSigned numericDesc (natural nonlexeme)
                              , floating = mkSignedFloating numericDesc positiveFloating
                              , unsignedCombined = mkUnsignedCombined numericDesc (natural nonlexeme) positiveFloating
                              , signedCombined = mkSignedCombined numericDesc (unsignedCombined nonlexeme)
                              }
        positiveFloating = mkUnsignedFloating numericDesc (natural nonlexeme) gen
        fully' p = whiteSpace space *> p <* eof
        fully p
          | Desc.whitespaceIsContextDependent spaceDesc = initSpace space *> fully' p
          | otherwise                                   = fully' p
        space = mkSpace spaceDesc

--TODO: better name for this, I guess?
type Lexeme :: *
data Lexeme = Lexeme
                { apply :: !(forall a. Parsec a -> Parsec a) -- this is tricky...
                , sym :: !(String -> Parsec ())
                , symbol :: !Symbol
                , names :: !Names
                , natural :: !(IntegerParsers CanHoldUnsigned)
                , integer :: !(IntegerParsers CanHoldSigned)
                , floating :: !FloatingParsers
                , unsignedCombined :: !CombinedParsers
                , signedCombined :: !CombinedParsers
                }
            | NonLexeme
                { sym :: !(String -> Parsec ())
                , symbol :: !Symbol
                , names :: !Names
                , natural :: !(IntegerParsers CanHoldUnsigned)
                , integer :: !(IntegerParsers CanHoldSigned)
                , floating :: !FloatingParsers
                , unsignedCombined :: !CombinedParsers
                , signedCombined :: !CombinedParsers
                }

type Space :: *
data Space = Space { whiteSpace :: !(Parsec ())
                   , skipComments :: !(Parsec ())
                   , alter :: forall a. Desc.CharPredicate -> Parsec a -> Parsec a
                   , initSpace :: Parsec ()
                   }

mkSpace :: Desc.SpaceDesc -> Space
mkSpace desc@Desc.SpaceDesc{..} = Space {..}
  where -- don't think we can trust doing initialisation here, it'll happen in some random order
        {-# NOINLINE wsImpl #-}
        !wsImpl = fromIORef (unsafePerformIO (newIORef (error "uninitialised space")))
        comment = commentParser desc -- do not make this lazy
        implOf
          | supportsComments desc = hide . maybe skipComments (skipMany . (<|> comment) . void . satisfy)
          | otherwise             = hide . maybe empty (skipMany . satisfy)
        !configuredWhitespace = implOf space
        !whiteSpace
          | whitespaceIsContextDependent = join (get wsImpl)
          | otherwise                    = configuredWhitespace
        !skipComments = skipMany comment
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
commentParser :: Desc.SpaceDesc -> Parsec ()
commentParser Desc.SpaceDesc{..} =
  require (multiEnabled || singleEnabled) "skipComments" noComments $
    require (not (multiEnabled && isPrefixOf commentStart commentLine)) "skipComments" noOverlap $
      hide (multiLine <|> singleLine)
  where
    -- can't make these strict until guard is gone
    openComment = atomic (string commentStart)
    closeComment = atomic (string commentEnd)
    multiLine = guard multiEnabled *> openComment *> wellNested 1
    wellNested :: Int -> Parsec ()
    wellNested 0 = unit
    wellNested n = closeComment *> wellNested (n - 1)
               <|> guard nestedComments *> openComment *> wellNested (n + 1)
               <|> item *> wellNested n
    singleLine = guard singleEnabled
              *> atomic (string commentLine)
              *> skipManyTill item endOfLineComment

    endOfLineComment
      | commentLineAllowsEOF = void endOfLine <|> eof
      | otherwise            = void endOfLine

    multiEnabled = not (null commentStart || null commentEnd)
    singleEnabled = not (null commentLine)
    noComments = "one of single- or multi-line comments must be enabled"
    noOverlap = "single-line comments must not overlap with multi-line comments"

supportsComments :: Desc.SpaceDesc -> Bool
supportsComments Desc.SpaceDesc{..} = not (null commentLine && null commentStart)

type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation
