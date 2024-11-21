{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances     #-}
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

keywords :: Set String
keywords = ["return"]

operators :: Set String
operators = ["+", "*", "-", "/", "="]

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


lexeme :: Lexer.Lexeme
lexeme = Lexer.lexeme lexer

keyword :: String -> Parsec ()
keyword = Lexer.softKeyword (Lexer.symbol lexeme)

operator :: String -> Parsec ()
operator = Lexer.softOperator (Lexer.symbol lexeme)

-- Generate the OverloadedStrings for the lexer.
$(overloadedStrings [| lexer |])

integer :: Parsec Int64
integer =
      wrap Lexer.decimal64
  <|> wrap Lexer.hexadecimal64
  <|> wrap Lexer.binary64
  where
    wrap p = fromInteger <$> p (Lexer.integer lexeme)

identifier :: Parsec String
identifier = Lexer.identifier (Lexer.names lexeme)


fully :: Parsec a -> Parsec a
fully = Lexer.fully lexer


alter :: (Char -> Bool) -> Parsec a -> Parsec a
alter = Lexer.alter (Lexer.space lexer) . Just
