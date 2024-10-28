{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-|
Module      : Text.Gigaparsec.Errors.Patterns
Description : This module contains combinators that help facilitate the error message generational patterns /Verified Errors/ and /Preventative Errors/.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module contains combinators that help facilitate the error message generational patterns /Verified Errors/™ and /Preventative Errors/™.
-}
module Text.Gigaparsec.Errors.Patterns (
    -- ** Verified Errors
    {-|
    These are combinators related to the /Verified Errors/ parser design pattern.

    They allow for the parsing of known illegal values, providing richer error messages in case they succeed.
    -}
    {-| ==== __Note__
    The following applies to each of the @verified\<...\>@ combinators:

    When this combinator fails (and not this parser itself), it will generate errors rooted 
    at the start of the parse (as if 'amend' had been used) and the caret will span the 
    entire successful parse of this parser.

    When this parser is not to be considered as a terminal error, use atomic around the entire
    combinator to allow for backtracking if this parser succeeds (and therefore fails).
    -}
    verifiedWith,
    verifiedFail, 
    verifiedUnexpected, 
    verifiedExplain,
    -- ** Preventative Errors
    {-|
    These are combinators related to the /Preventative Errors/ parser design pattern.

    They allow for the parsing of known illegal values, providing richer error messages in case they succeed.
    -}
    {-| ==== __Note__
    The following applies to each of the @verified\<...\>@ combinators:

    When this combinator fails (and not this parser itself), it will generate errors rooted 
    at the start of the parse (as if 'amend' had been used) and the caret will span the 
    entire successful parse of this parser.

    When this parser is not to be considered as a terminal error, use atomic around the entire
    combinator to allow for backtracking if this parser succeeds (and therefore fails).
    -}
    preventWith,
    preventativeFail, 
    preventativeExplain
  ) where

import Text.Gigaparsec (Parsec, atomic, (<+>), unit)
import Text.Gigaparsec.Position (withWidth)
import Text.Gigaparsec.Errors.Combinator (amend, hide)
import Text.Gigaparsec.Errors.ErrorGen (ErrorGen)
import Text.Gigaparsec.Errors.ErrorGen qualified as ErrorGen (
    asFail, asSelect, UnexpectedItem(RawItem),
    vanillaGen, specializedGen, unexpected, reason, messages
  )

{-|
Ensures this parser does not succeed, failing with an error as described by the given 'ErrorGen' object.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an error message using the given 'ErrorGen' with width the same as the parsed data. 
However, if this parser fails, no input is consumed and an empty error is generated. 

This parser will produce no labels if it fails.

@since 0.2.3.0
-}
verifiedWith :: ErrorGen a -- ^ @err@, the generator that produces the error message.
             -> Parsec a   -- ^ @p@, the parser for the bad input.
             -> Parsec b   -- ^ a parser that ensures @p@ fails, otherwise it raises an error described by @err@.
verifiedWith err p = amend (ErrorGen.asFail err (hide (withWidth (atomic p))))

verifiedWithVanilla :: (a -> ErrorGen.UnexpectedItem) -> (a -> Maybe String) -> Parsec a -> Parsec b
verifiedWithVanilla unexGen reasonGen = verifiedWith $
  ErrorGen.vanillaGen {
    ErrorGen.unexpected = unexGen,
    ErrorGen.reason = reasonGen
  }

verifiedWithVanillaRaw :: (a -> Maybe String) -> Parsec a -> Parsec b
verifiedWithVanillaRaw = verifiedWithVanilla (const ErrorGen.RawItem)

{-|
Ensures this parser does not succeed, failing with a specialised error based on this parsers result if it does.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an error message based on the parsed result. 
However, if this parser fails, no input is consumed and an empty error is generated. 

This parser will produce no labels if it fails.

@since 0.2.3.0
-}
verifiedFail :: (a -> [String]) -- ^ the function that generates the error messages from the parsed value.
             -> Parsec a        -- ^ @p@, the parser for the bad input.
             -> Parsec b        -- ^ a parser that ensures @p@ fails, otherwise it raises an error with 
                                -- the given message based on the result.
verifiedFail msggen = verifiedWith $
  ErrorGen.specializedGen {
    ErrorGen.messages = msggen
  }

{-|
Ensures this parser does not succeed, failing with a vanilla error with an unexpected message 
and caret spanning the parse.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an unexpected message the same width as the parse. 
However, if this parser fails, no input is consumed and an empty error is generated. 

This parser will produce no labels if it fails.

@since 0.2.3.0
-}
verifiedUnexpected :: Parsec a -- ^ @p@, the parser for the bad input.
                   -> Parsec b -- ^ a parser that ensures @p@ fails, otherwise it raises an unexpected error.
verifiedUnexpected = verifiedWithVanillaRaw (const Nothing)

{-|
Ensures this parser does not succeed, failing with a vanilla error with an unexpected message 
and caret spanning the parse and a reason generated from this parser's result.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an unexpected message the same width as the parse along with a reason generated from the successful parse. 
However, if this parser fails, no input is consumed and an empty error is generated. 
This parser will produce no labels if it fails.

@since 0.2.3.0
-}
verifiedExplain :: (a -> String) -- ^ a function that produces a reason for the error given the parsed result.
                -> Parsec a      -- ^ @p@, the parser for the bad input.
                -> Parsec b      -- ^ a parser that ensures @p@ fails, otherwise it raises an error with 
                                 -- the given reason based on the result.
verifiedExplain reasongen = verifiedWithVanillaRaw (Just . reasongen)

{-|
Ensures this parser does not succeed, failing with an error as described by the given ErrorGen object.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an error message using the given errGen with width the same as the 
parsed data along with the given labels. 
However, if this parser fails, no input is consumed and this combinator succeeds. 
  
This parser will produce no evidence of running if it succeeds.

@since 0.2.3.0
-}
preventWith :: ErrorGen a -- ^ @err@, the generator that produces the error message.
            -> Parsec a   -- ^ @p@, the parser for the bad input.
            -> Parsec ()  -- ^ a parser that ensures @p@ fails, otherwise it raises an error described by @err@.
preventWith err p = amend (ErrorGen.asSelect err (withWidth (hide (atomic p)) <+> unit))

preventWithVanilla :: (a -> ErrorGen.UnexpectedItem) -> (a -> Maybe String) -> Parsec a -> Parsec ()
preventWithVanilla unexGen reasonGen = preventWith $
  ErrorGen.vanillaGen {
    ErrorGen.unexpected = unexGen,
    ErrorGen.reason = reasonGen
  }

preventWithVanillaRaw :: (a -> Maybe String) -> Parsec a -> Parsec ()
preventWithVanillaRaw = preventWithVanilla (const ErrorGen.RawItem)

{-|
Ensures this parser does not succeed, failing with a specialised error based on this parsers result if it does.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an error message based on the parsed result. 
However, if this parser fails, no input is consumed and this combinator succeeds. 
This parser will produce no evidence of running if it succeeds.

@since 0.2.3.0
-}
preventativeFail :: (a -> [String]) -- ^ the function that generates the error messages from the parsed value.
                 -> Parsec a        -- ^ @p@, the parser for the bad input.
                 -> Parsec ()       -- ^ a parser that ensures @p@ fails, otherwise it raises an error with 
                                    -- the given message based on the result.
preventativeFail msggen = preventWith $
  ErrorGen.specializedGen {
    ErrorGen.messages = msggen
  }

{-|
Ensures this parser does not succeed, failing with a vanilla error with an unexpected message 
and caret spanning the parse and a reason generated from this parser's result.

If this parser succeeds, input is consumed and this combinator will fail, 
producing an unexpected message the same width as the parse along with a reason generated 
from the successful parse along with the given labels. 
However, if this parser fails, no input is consumed and this combinator succeeds. 
This parser will produce no evidence of running if it succeeds.

@since 0.2.3.0
-}
preventativeExplain :: (a -> String) -- ^ a function that produces a reason for the error given the parsed result.
                    -> Parsec a      -- ^ @p@, the parser for the bad input.
                    -> Parsec ()     -- ^ a parser that ensures @p@ fails, otherwise it raises an unexpected error with 
                                     -- the given reason based on the result.
preventativeExplain reasongen = preventWithVanillaRaw (Just . reasongen)
