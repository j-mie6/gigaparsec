{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Text.Gigaparsec.Errors.TokenExtractors
Description : This module contains implementations of token extractors that can be used in the
              @ErrorBuilder@ to decide how to extract unexpected tokens from the residual input left
              over from a parse error.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains implementations of token extractors that can be used in the
"Text.Gigaparsec.Errors.ErrorBuilder" to decide how to extract unexpected tokens from the residual
input left over from a parse error.

These are common strategies, and something here is likely to be what is needed. They are all careful
to handle unprintable characters and whitespace in a sensible way, and account for unicode codepoints
that are wider than a single 16-bit character.

@since 0.2.5.0
-}
module Text.Gigaparsec.Errors.TokenExtractors (
    Token(..), TokenExtractor,
    tillNextWhitespace,
    singleChar,
    matchParserDemand,
    lexToken, lexTokenWithSelect
  ) where

import Text.Gigaparsec (
    Parsec, Result(Success), parse,
    atomic, lookAhead, notFollowedBy, some, (<+>), (<~>), mapMaybeS
  )
import Text.Gigaparsec.Char (item)
import Text.Gigaparsec.Combinator (option)
import Text.Gigaparsec.Position (offset)

import Data.Char (generalCategory, ord, GeneralCategory(Format, Surrogate, PrivateUse, NotAssigned, Control))
import Data.Char qualified as Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Void (Void)
import Data.Foldable (maximumBy)
import Numeric (showHex)
import Data.Function (on)
import Data.Maybe (catMaybes)

{-|
Type alias for token extractors, matches the shape of
'Text.Gigaparsec.Errors.ErrorBuilder.unexpectedToken'.

@since 0.2.5.0
-}
type TokenExtractor :: *
type TokenExtractor = NonEmpty Char -- ^ the remaining input, @cs@, at point of failure.
                    -> Word         -- ^ the input the parser tried to read when it failed
                                    --   (this is __not__ guaranteed to be smaller than the length of
                                    --    @cs@, but is __guaranteed to be greater than 0__).
                    -> Bool         -- ^ was this error generated as part of \"lexing\", or in a wider parser (see 'Text.Gigaparsec.Errors.Combinator.markAsToken').
                    -> Token        -- ^ a token extracted from @cs@ that will be used as part of the unexpected message.

{-|
This type represents an extracted token returned by 'Text.Gigaparsec.Errors.ErrorBuilder.unexpectedToken'
in 'Text.Gigaparsec.Errors.ErrorBuilder.ErrorBuilder'.

There is deliberately no analogue for @EndOfInput@ because we guarantee that non-empty
residual input is provided to token extraction.

@since 0.2.5.0
-}
type Token :: *
data Token = Raw                   -- ^ This is a token that is directly extracted from the residual input itself.
              !String              -- ^ the input extracted.
           | Named                 -- ^ This is a token that has been given a name, and is treated like a labelled item.
              !String              -- ^ the description of the token.
              {-# UNPACK #-} !Word -- ^ the amount of residual input this token ate.

{-# INLINABLE tillNextWhitespace #-}
{-|
This extractor provides an implementation for 'Text.Gigaparsec.ErrorBuilder.unexpectedToken':
it will construct a token that extends to the next available whitespace in the remaining input.
It can be configured to constrict this token to the minimum of the next whitespace or whatever the
parser demanded.

In the case of unprintable characters or whitespace, this extractor will favour reporting a more
meaningful name.

@since 0.2.5.0
-}
tillNextWhitespace :: Bool           -- ^ should the extractor cap the token to the amount of input the parser demanded?
                   -> (Char -> Bool) -- ^ what counts as a space character
                   -> TokenExtractor
tillNextWhitespace _ _ (whitespaceOrUnprintable -> Just tok) _ _ = tok
tillNextWhitespace trimToDemand isSpace (c :| cs) parserDemanded _
  | trimToDemand = Raw (take (fromIntegral parserDemanded) (tillSpace (c:cs)))
  | otherwise    = Raw (tillSpace (c:cs))
  where tillSpace = takeWhile (not . isSpace)

{-|
This extractor provides an implementation for 'Text.Gigaparsec.ErrorBuilder.unexpectedToken':
it will unconditionally report the first character in the remaining input as the problematic token.

In the case of unprintable characters or whitespace, this extractor will favour reporting
a more meaningful name.

@since 0.2.5.0
-}
{-# INLINABLE singleChar #-}
singleChar :: TokenExtractor
singleChar (whitespaceOrUnprintable -> Just tok) _ _ = tok
singleChar (c :| _) _ _ = Raw [c]

{-|
This extractor provides an implementation for 'Text.Gigaparsec.ErrorBuilder.unexpectedToken':
it will make a token as wide as the amount of input the parser tried to consume when it failed.

In the case of unprintable characters or whitespace, this extractor will favour reporting a more
meaningful name.

@since 0.2.5.0
-}
{-# INLINABLE matchParserDemand #-}
matchParserDemand :: TokenExtractor
matchParserDemand (whitespaceOrUnprintable -> Just tok) _ _ = tok
matchParserDemand (c :| cs) parserDemanded _ = Raw (take (fromIntegral parserDemanded) (c:cs))

{-# INLINABLE whitespaceOrUnprintable #-}
whitespaceOrUnprintable :: NonEmpty Char -> Maybe Token
whitespaceOrUnprintable ('\n' :| _) = Just $ Named "newline" 1
whitespaceOrUnprintable ('\r' :| _) = Just $ Named "carriage return" 1
whitespaceOrUnprintable ('\t' :| _) = Just $ Named "tab" 1
whitespaceOrUnprintable (' ' :| _) = Just $ Named "space" 1
whitespaceOrUnprintable (c :| _)
  | Char.isSpace c = Just $ Named "whitespace character" 1
  | otherwise      = case generalCategory c of
    Format -> unprintable
    Surrogate -> unprintable
    PrivateUse -> unprintable
    NotAssigned -> unprintable
    Control -> unprintable
    _ -> Nothing
  where unprintable = Just $ Named ("non-printable character (\\x" ++ showHex (ord c) ")") 1

{-|
This extractor provides an implementation for 'Text.Gigaparsec.ErrorBuilder.unexpectedToken':
it will try and parse the residual input to identify a valid lexical token to report.

When parsing a grammar that as a dedicated lexical distinction, it is nice to be able to report
problematic tokens relevant to that grammar as opposed to generic input lifted straight from the
input stream. The easiest way of doing this would be having a pre-lexing pass and parsing based
on tokens, but this is deliberately not how Parsley is designed. Instead, this extractor can
try and parse the remaining input to try and identify a token on demand.

If the @lexicalError@ flag of the @unexpectedToken@ function is not set, which would indicate a
problem within a token reported by a classical lexer and not the parser, the extractor will
try to parse each of the provided @tokens@ in turn: whichever is the longest matched of these
tokens will be reported as the problematic one, where an earlier token arbitrates ties
('lexTokenWithSelect' can alter which is chosen). For best effect, these tokens should not consume
whitespace (which would otherwise be included at the end of the token!): this means that, if
using the @Lexer@, the functionality in __@nonlexeme@__ should be used. If one of the
givens tokens cannot be parsed, the input until the /next/ valid parsable token (or end of input)
is returned as a @Raw@.

If @lexicalError@ is true, then the given token extractor will be used instead to extract a
default token.

@since 0.2.5.0
-}
{-# INLINABLE lexToken #-}
lexToken :: [Parsec String] -- ^ The tokens that should be recognised by this extractor: each parser should return the
                            -- intended name of the token exactly as it should appear in the "Named" token.
                            --
                            -- This /should/ include a whitespace parser for "unexpected whitespace". However, with the
                            -- exception of the whitespace parser, these tokens should not consume trailing (and
                            -- certainly not leading) whitespace: if using definitions from "Text.Gigaparsec.Token.Lexer"
                            -- functionality, the @nonlexeme@ versions of the tokens should be used.
         -> TokenExtractor  -- ^ If the parser failed during the parsing of a token, this
                            -- function extracts the problematic item from the remaining input.
         -> TokenExtractor
lexToken = lexTokenWithSelect (maximumBy (compare `on` snd))

{-|
This extractor provides an implementation for 'Text.Gigaparsec.ErrorBuilder.unexpectedToken':
it will try and parse the residual input to identify a valid lexical token to report.

When parsing a grammar that as a dedicated lexical distinction, it is nice to be able to report
problematic tokens relevant to that grammar as opposed to generic input lifted straight from the
input stream. The easiest way of doing this would be having a pre-lexing pass and parsing based
on tokens, but this is deliberately not how Parsley is designed. Instead, this extractor can
try and parse the remaining input to try and identify a token on demand.

If the @lexicalError@ flag of the @unexpectedToken@ function is not set, which would indicate a
problem within a token reported by a classical lexer and not the parser, the extractor will
try to parse each of the provided @tokens@ in turn: the given function is used to select which
is returned.
For best effect, these tokens should not consume
whitespace (which would otherwise be included at the end of the token!): this means that, if
using the @Lexer@, the functionality in __@nonlexeme@__ should be used. If one of the
givens tokens cannot be parsed, the input until the /next/ valid parsable token (or end of input)
is returned as a @Raw@.

If @lexicalError@ is true, then the given token extractor will be used instead to extract a
default token.

@since 0.2.5.0
-}
{-# INLINABLE lexTokenWithSelect #-}
lexTokenWithSelect :: (NonEmpty (String, Word) -> (String, Word))
                   -- ^ If the extractor is successful in identifying tokens that can be parsed from
                   -- the residual input, this function will select /one/ of them to report back.
                   -> [Parsec String]
                   -- ^ The tokens that should be recognised by this extractor: each parser should return the
                   -- intended name of the token exactly as it should appear in the "Named" token.
                   --
                   -- This /should/ include a whitespace parser for "unexpected whitespace". However, with the
                   -- exception of the whitespace parser, these tokens should not consume trailing (and
                   -- certainly not leading) whitespace: if using definitions from "Text.Gigaparsec.Token.Lexer"
                   -- functionality, the @nonlexeme@ versions of the tokens should be used.
                   -> TokenExtractor
                   -- ^ If the parser failed during the parsing of a token, this
                   -- function extracts the problematic item from the remaining input.
                   -> TokenExtractor
lexTokenWithSelect selectToken tokens extractItem cs parserDemanded lexical
  | lexical = extractItem cs parserDemanded True
  | otherwise = extractToken cs
  where
    extractToken :: NonEmpty Char -> Token
    extractToken inp =
      let Success rawOrToks = parse @Void parser (NonEmpty.toList inp)
      in either (uncurry Named . selectToken) Raw rawOrToks

    parser :: Parsec (Either (NonEmpty (String, Word)) String)
    parser =
      let toks = mapMaybeS (nonEmpty . catMaybes)
                           (traverse (\t -> option (lookAhead (atomic t <~> offset))) tokens)
          rawTok = some (traverse notFollowedBy tokens *> item)
      in toks <+> rawTok
