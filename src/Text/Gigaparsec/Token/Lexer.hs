{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstraintKinds, DataKinds #-}
-- Ideally, we should probably expose all the functionally via this one file
-- for ergonomics
{-|
Module      : Text.Gigaparsec.Token.Lexer
Description : This module provides a large selection of functionality concerned with lexing.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module provides a large selection of functionality concerned with lexing.

In traditional compilers, lexing and parsing are two largely separate processes;
lexing turns raw input into a series of tokens, and parsing then processes these tokens.
Parser combinators, on the other hand, are often implemented to deal directly with the input stream.

Nonetheless, a lexer abstraction may be achieved by defining a core set of /lexing/ combinators that convert input to tokens,
and then defining the /parsing/ combinators in terms of these.
The parsers defined using 'Lexer' construct these /lexing/ combinators, which creates a clear and logical separation from the rest of the parser.

It is possible that some of the implementations of parsers found within this class may have been hand-optimised for performance:
care will have been taken to ensure these implementations precisely match the semantics of the originals.

-}
module Text.Gigaparsec.Token.Lexer (
    -- ** Lexing
    Lexer,
    mkLexer,
    mkLexerWithErrorConfig,
    -- ** Lexemes and Non-Lexemes
    {-|
    A key distinction in lexers is between lexemes and non-lexemes:

    * 'lexeme' consumes whitespace.
    It should be used by a wider parser, to ensure whitespace is handled uniformly.
    The output of 'lexeme' can be considered a /token/ as provided by traditional lexers, and can be used by the parser.
    * 'nonlexeme' does not consume whitespace.
    It should be used to define further composite tokens or in special circumstances where whitespace should not be consumed.
    One may consider the output of 'nonlexeme' to still be in the /lexing/ stage of parsing, and not necessarily a valid token.
    -}

    -- *** Lexemes
    {-|
    Ideally, a wider parser should not be concerned with handling whitespace,
    as it is responsible for dealing with a stream of tokens.
    With parser combinators, however, it is usually not the case that there is a separate distinction between the parsing phase and the lexing phase.
    That said, it is good practice to establish a logical separation between the two worlds.
    As such, 'lexeme' contains parsers that parse tokens, and these are whitespace-aware.
    This means that whitespace will be consumed after any of these parsers are parsed.
    It is not required that whitespace be present.
    -}
    lexeme,
    -- *** Non-Lexemes
    {-|
    Whilst the functionality in lexeme is strongly recommended for wider use in a parser, the functionality here may be useful for more specialised use-cases.
    In particular, these may for the building blocks for more complex tokens (where whitespace is not allowed between them, say),
    in which case these compound tokens can be turned into lexemes manually.

    For example, the lexer does not have configuration for trailing specifiers on numeric literals (like, 1024L in Scala, say):
    the desired numeric literal parser could be extended with this functionality before whitespace is consumed by using the variant found in this object.

    These tokens can also be used for lexical extraction,
    which can be performed by the ErrorBuilder typeclass:
    this can be used to try and extract tokens from the input stream when an error happens,
    to provide a more informative error.
    In this case, it is desirable to not consume whitespace after the token to keep
    the error tight and precise.
    -}
    nonlexeme,
    -- *** Fully and Space
    fully, space,
    -- *** 'Lexeme' Fields
    {-|
    Despite their differences, lexemes and non-lexemes share a lot of common functionality.
    The type 'Lexeme' describes both lexemes and non-lexemes, so that this common functionality may be exploited.
    -}
    Lexeme,
    {-|
    Lexemes and Non-Lexemes are described by these common fields.
    -}
    apply, sym, symbol, names,
    -- ** Symbolic Tokens
    {-|
    The 'Symbol' interface handles the parsing of symbolic tokens, such as keywords.
    -}
    Symbol, softKeyword, softOperator,
    -- ** Name Tokens
    {-|
    The 'Names' interface handles the parsing of identifiers and operators.
    -}
    Names, identifier, identifier', userDefinedOperator, userDefinedOperator',
    -- ** Numeric Tokens
    {-|
    These types and combinators parse numeric literals, such as integers and reals.
    -}

    CanHoldSigned, CanHoldUnsigned,
    -- *** Integer Parsers
    {-|
    'IntegerParsers' handles integer parsing (signed and unsigned).
    This is mainly used by the combinators 'integer' and 'natural'.
    -}
    IntegerParsers,
    integer, natural,
    -- **** Fixed-Base Parsers
    decimal, hexadecimal, octal, binary,
    -- **** Fixed-Width Numeric Tokens
    {-|
    These combinators tokenize numbers that must be within specific bit-widths.
    The possible bit-widths are provided by 'Text.Gigaparsec.Internal.Token.BitBounds.Bits'.
    -}
    -- ***** Decimal Tokens
    decimal8, decimal16, decimal32, decimal64,
    -- ***** Hexadecimal Tokens
    hexadecimal8, hexadecimal16, hexadecimal32, hexadecimal64,
    -- ***** Octal Tokens
    octal8, octal16, octal32, octal64,
    -- ***** Binary Tokens
    binary8, binary16, binary32, binary64,
    -- ** Textual Tokens
    {-|
    The 'TextParsers' interface handles the parsing of string and character literals.
    -}
    TextParsers,
    ascii, unicode, latin1,
    -- *** String Parsers
    {-|
    A 'Lexer' provides the following 'TextParsers' for string literals.
    -}
    stringLiteral, rawStringLiteral, multiStringLiteral, rawMultiStringLiteral,
    -- *** Character Parsers
    {-|
    A 'Lexer' provides the following 'TextParsers' for character literals.
    -}
    charLiteral,
    -- ** Whitespace and Comments
    {-|
    'Space' and its fields are concerned with special treatment of whitespace itself.

    Most of the time, the functionality herein will not be required,
    as 'lexeme' and 'fully' will consistently handle whitespace.

    However, whitespace /is/ significant in some languages, like Python and Haskell,
    in which case 'Space' provides a way to control how whitespace is consumed.
    -}
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
