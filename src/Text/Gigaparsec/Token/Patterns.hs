{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{-|
Module      : Text.Gigaparsec.Token.Patterns
Description : Template Haskell generators to help with patterns
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module is currently experimental, and may have bugs depending on the version
of Haskell, or the extensions enabled. Please report any issues to the maintainers.

@since 0.2.2.0
-}
module Text.Gigaparsec.Token.Patterns (
  -- * Overloaded Strings
  overloadedStrings,
  -- * Lexer Combinators
  {-|
  These functions will generate combinators for parsing things like identifiers, keywords, etc,
  as described by a 'Lexer'.

  The combinators will behave like their counterparts in "Text.Gigaparsec.Token.Lexer",
  except they do not need to be given a lexer(/a subcomponent of a lexer) as an argument.
  
  * 'lexerCombinators' will generate these lexer combinators using the same name as the original combinators.
  * 'lexerCombinatorsWithNames' lets you rename the generated combinator; otherwise it behaves exactly as 'lexerCombinators'.

  The combinators for numeric literals need their own generation function `generateIntegerParsers`.
  If you try to generate a `Text.Gigaparsec.Token.Lexer.decimal` parser using `lexerCombinators`,
  you will get an error.
  
  ==== __Examples:__

  The combinator "Text.Gigaparsec.Token.Lexer.identifier" is used for parsing identifiers, and has the type,

  > Lexer.identifier :: Lexer -> Parsec String

  It is annoying to have to feed the lexer as the initial argument, as this will be fixed throughout the parser.
  Usually, one ends up writing their own combinator:

  > identifier :: Parsec String
  > identifier = Lexer.identifier lexer

  Writing these by hand is tedious; especially if we wish to use multiple such combinators.
  This is where `lexerCombinators` comes in:

  > $(lexerCombinators [| lexer |] ['Lexer.identifier])

  will generate the combinator,

  > identifier :: Parsec String
  > identifier = Lexer.identifier lexer

  If we wish to use multiple combinators, we just add each one to the list.
  For example,

  > $(lexerCombinators [| lexer |] ['Lexer.identifier, 'Lexer.fully, 'Lexer.softKeyword, 'Lexer.softOperator])


  -}
  lexerCombinators,
  lexerCombinatorsWithNames,
  -- ** Integer Parsers
  generateIntegerParsers,
  -- *** IntegerParserConfig
  IntegerParserConfig,
  prefix, widths, bases, includeUnbounded, signedOrUnsigned, collatedParser,
  -- **** Presets
  emptyIntegerParserConfig,
  emptySignedIntegerParserConfig,
  emptyUnsignedIntegerParserConfig,
  -- **** Associated Types
  SignedOrUnsigned(..),
  allBases,
  IntLitBase(..),
  ) where

import Text.Gigaparsec (Parsec)
import safe Text.Gigaparsec.Internal.Token.Lexer
    ( Lexeme(sym),
      Lexer(lexeme) )
import safe Text.Gigaparsec.Internal.Token.Patterns.IntegerParsers
    ( IntegerParserConfig(collatedParser, prefix, widths, bases,
                          includeUnbounded, signedOrUnsigned),
      SignedOrUnsigned(..),
      allBases,
      IntLitBase(..),
      generateIntegerParsers,
      emptyIntegerParserConfig,
      emptySignedIntegerParserConfig,
      emptyUnsignedIntegerParserConfig )


import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax (Q, Dec (..), Exp)




import Text.Gigaparsec.Internal.Token.Patterns.LexerCombinators (lexerCombinators, lexerCombinatorsWithNames)

{-|
When given a quoted reference to a 'Text.Gigaparsec.Token.Lexer.Lexer', for example
@[|lexer|]@, this function will synthesise an `IsString` instance that will
allow string literals to serve as @Parsec ()@. These literals will parse symbols
in the language associated with the lexer, followed by consuming valid whitespace.

@since 0.2.2.0
-}
overloadedStrings :: Q Exp   -- ^ the quoted 'Text.Gigaparsec.Token.Lexer.Lexer'
                  -> Q [Dec] -- ^ a synthesised `IsString` instance.
overloadedStrings qlexer = [d|
    instance u ~ () => IsString (Parsec u) where
      fromString = sym (lexeme $qlexer) -- TODO: one day, $qlexer.lexeme.sym
  |]

