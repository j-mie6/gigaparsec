{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-|
This module describes the Lexer for @ExprLang@.
This lexer description need not be exposed to the other modules, so instead we
expose some primitive 'lexing' combinators defined in terms of a lexer description.
-}
module ExprLang.Lexer (
  keywords,
  operators,
  keyword,
  operator,
  integer,
  identifier,
  fully,
  alter
  ) where
import qualified Text.Gigaparsec.Token.Descriptions  as Desc
import qualified Text.Gigaparsec.Token.Lexer         as Lexer

import           Data.Char                           (generalCategory, isAlpha,
                                                      isAlphaNum, isSpace)
import qualified Data.Char                           as Char (GeneralCategory (Space))
import           Data.Int                            (Int64)
import           Data.List.NonEmpty                  (NonEmpty)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.String                         (IsString (fromString))
import           Data.Word                           (Word64)
import           Text.Gigaparsec                     (Parsec, Result(..),
                                                      parseFromFile, (<|>), ($>))
import           Text.Gigaparsec.Char                (newline, string, satisfy)
import           Text.Gigaparsec.Combinator           (endBy1, optional)
import           Text.Gigaparsec.Expr                (Fixity (InfixL),
                                                      Prec (Atom), precedence,
                                                      sops, (+<))
import           Text.Gigaparsec.Patterns            (deriveDeferredConstructors,
                                                      deriveLiftedConstructors)
import           Text.Gigaparsec.Token.Errors        (Bits)
import           Text.Gigaparsec.Token.Patterns      (overloadedStrings)
import Data.List (intercalate, intersperse)

-- | The reserved keywords of the language
keywords :: Set String
keywords = ["return"]

{-| The reserved operators of the language.

This includes the binary operators in expressions, as well as the assignment operator '='.
-}
operators :: Set String
operators = ["+", "*", "-", "/", "="]

{-|
The lexer generated from a lexical description, which provide an abstract way of turning input
into 'tokens'.

All other lexing combinators should be defined in terms of this.
-}
lexer :: Lexer.Lexer
lexer = Lexer.mkLexer $ Desc.plain {
    Desc.nameDesc = nameDesc,
    Desc.symbolDesc = symbolDesc,
    Desc.numericDesc = numericDesc,
    Desc.textDesc = textDesc,
    Desc.spaceDesc = spaceDesc
  }
  where
    nameDesc = Desc.plainName {
        Desc.identifierStart = Just isAlpha,
        Desc.identifierLetter = Just isAlphaNum
      }
    symbolDesc = Desc.plainSymbol {
        Desc.hardKeywords = keywords,
        Desc.hardOperators = operators,
        Desc.caseSensitive = True
      }
    numericDesc = Desc.plainNumeric
    textDesc = Desc.plainText
    spaceDesc = Desc.plainSpace {
        Desc.lineCommentStart = "//",
        Desc.lineCommentAllowsEOF = True,
        Desc.multiLineCommentStart = "/*",
        Desc.multiLineCommentEnd = "*/",
        Desc.multiLineNestedComments = False,
        Desc.space = Just ((Char.Space ==) . generalCategory),
        Desc.whitespaceIsContextDependent = True
      }

-- | The 'Lexeme' component of the 'lexer'
{-# INLINE lexeme #-}
lexeme :: Lexer.Lexeme
lexeme = Lexer.lexeme lexer

{-| Parses the given string as a single \'keyword\' token.
-} 
{-# INLINE keyword #-}
keyword :: String -> Parsec ()
keyword = Lexer.softKeyword (Lexer.symbol lexeme)

{-| Parses the given string as an \'operator\' token.
-} 
{-# INLINE operator #-}
operator :: String -> Parsec ()
operator = Lexer.softOperator (Lexer.symbol lexeme)

-- Generate the OverloadedStrings for the lexer.
$(overloadedStrings [| lexer |])

{-| Parses a single \'integer\' token.

This may be written in decimal, hexadecimal, or binary form.
-}
{-# INLINE integer #-}
integer :: Parsec Int64
integer =
      wrap Lexer.decimal64
  <|> wrap Lexer.hexadecimal64
  <|> wrap Lexer.binary64
  where
    -- Have to process the output as gigaparsec doesn't yet support `Int64` as a valid output
    -- type of the numeric processors of the lexer.
    {-# INLINE wrap #-}
    wrap p = fromInteger <$> p (Lexer.integer lexeme)

{-| Parses a single \'identifier\' token.

-}
identifier :: Parsec String
identifier = Lexer.identifier (Lexer.names lexeme)

{-| Ensures the given parser @p@ consumes all input.

This *must* be used by any top-level parsers, as 'Lexer.fully' will initialise the
lexer and its various components before running @p@.
-}
fully :: Parsec a -> Parsec a
fully = Lexer.fully lexer

{-| Change how whitespace is parsed while running the given parser @p@.
The given predicate @pred@ will return 'True' for what characters are considered whitespace.
-}
alter :: (Char -> Bool) 
      -- ^ @pred@, defines what characters are to be considered whitespace when running @p@.
      -> Parsec a 
      -- ^ @p@, the parser to run with the altered definition of whitespace, according to @pred@.
      -> Parsec a
      -- ^ a parser that runs @p@ with the changed definition of whitespace.
alter = Lexer.alter (Lexer.space lexer) . Just
