{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstraintKinds, DataKinds #-}
-- Ideally, we should probably expose all the functionally via this one file
-- for ergonomics
module Text.Gigaparsec.Token.Lexer (
    Lexer, mkLexer, mkLexerWithErrorConfig,
    Lexeme, lexeme, nonlexeme, fully, space,
    apply, sym, symbol, names,
    -- Symbol
    Symbol, softKeyword, softOperator,
    -- Names
    Names, identifier, identifier', userDefinedOperator, userDefinedOperator',
    -- Numeric
    IntegerParsers, CanHoldSigned, CanHoldUnsigned,
    integer, natural, decimal, hexadecimal, octal, binary,
    decimal8, decimal16, decimal32, decimal64,
    hexadecimal8, hexadecimal16, hexadecimal32, hexadecimal64,
    octal8, octal16, octal32, octal64,
    binary8, binary16, binary32, binary64,
    -- Text
    TextParsers,
    stringLiteral, rawStringLiteral, multiStringLiteral, rawMultiStringLiteral,
    charLiteral,
    ascii, unicode, latin1,
    -- Space
    Space, skipComments, whiteSpace, alter, initSpace,
  ) where

import Text.Gigaparsec.Internal.Token.Lexer (
    Lexer, mkLexer, mkLexerWithErrorConfig,
    Lexeme (rawMultiStringLiteral), lexeme, nonlexeme, fully, space,
    apply, sym, symbol, names,
    integer, natural,
    stringLiteral, rawStringLiteral, multiStringLiteral, rawMultiStringLiteral,
    charLiteral,
    Space, skipComments, whiteSpace, alter, initSpace
  )
import Text.Gigaparsec.Internal.Token.Symbol (Symbol, softKeyword, softOperator)
import Text.Gigaparsec.Internal.Token.Names (
    Names, identifier, identifier', userDefinedOperator, userDefinedOperator'
  )
import Text.Gigaparsec.Internal.Token.Numeric (
    IntegerParsers, decimal, hexadecimal, octal, binary,
    decimal8, decimal16, decimal32, decimal64,
    hexadecimal8, hexadecimal16, hexadecimal32, hexadecimal64,
    octal8, octal16, octal32, octal64,
    binary8, binary16, binary32, binary64
  )
import Text.Gigaparsec.Internal.Token.BitBounds qualified as Internal
import Text.Gigaparsec.Internal.Token.Text (
    TextParsers, ascii, unicode, latin1
  )

import Data.Kind (Constraint)

type CanHoldSigned :: Internal.Bits -> * -> Constraint
type CanHoldSigned = Internal.CanHoldSigned
type CanHoldUnsigned :: Internal.Bits -> * -> Constraint
type CanHoldUnsigned = Internal.CanHoldUnsigned
