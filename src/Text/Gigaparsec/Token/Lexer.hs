{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- Ideally, we should probably expose all the functionally via this one file
-- for ergonomics
module Text.Gigaparsec.Token.Lexer (
    Lexer, mkLexer,
    Lexeme, lexeme, nonlexeme, fully, space,
    apply, sym, symbol, names, -- more go here, no numeric and no text
    -- Symbol
    Symbol, softKeyword, softOperator,
    -- Names
    Names, identifier, identifier', userDefinedOperator, userDefinedOperator',
    -- Numeric
    IntegerParsers, CanHoldSigned, CanHoldUnsigned,
    integer, natural,
    -- Text
    CharacterParsers, StringParsers,
    stringLiteral, rawStringLiteral, multiStringLiteral, rawMultiStringLiteral,
    charLiteral,
    -- Space
    Space, skipComments, whiteSpace, alter, initSpace,
  ) where

import Text.Gigaparsec.Internal.Token.Lexer
import Text.Gigaparsec.Internal.Token.Symbol (Symbol, softKeyword, softOperator)
import Text.Gigaparsec.Internal.Token.Names (
    Names, identifier, identifier', userDefinedOperator, userDefinedOperator'
  )
import Text.Gigaparsec.Internal.Token.Numeric (
    IntegerParsers,
    CanHoldSigned, CanHoldUnsigned
  )
import Text.Gigaparsec.Internal.Token.Text (
    CharacterParsers, StringParsers,
  )
