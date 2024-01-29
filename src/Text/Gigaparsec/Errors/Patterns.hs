{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Text.Gigaparsec.Errors.Patterns (
    verifiedWith,
    verifiedFail, verifiedUnexpected, verifiedExplain,
    preventWith,
    preventativeFail, preventativeExplain
  ) where

import Text.Gigaparsec (Parsec, atomic, (<+>), unit)
import Text.Gigaparsec.Position (withWidth)
import Text.Gigaparsec.Errors.Combinator (amend, hide)
import Text.Gigaparsec.Errors.ErrorGen (ErrorGen)
import Text.Gigaparsec.Errors.ErrorGen qualified as ErrorGen (
    asFail, asSelect, UnexpectedItem(RawItem),
    vanillaGen, specializedGen, unexpected, reason, messages
  )

{-
@since 0.2.3.0
-}
verifiedWith :: ErrorGen a -> Parsec a -> Parsec b
verifiedWith err p = amend (ErrorGen.asFail err (hide (withWidth (atomic p))))

verifiedWithVanilla :: (a -> ErrorGen.UnexpectedItem) -> (a -> Maybe String) -> Parsec a -> Parsec b
verifiedWithVanilla unexGen reasonGen = verifiedWith $
  ErrorGen.vanillaGen {
    ErrorGen.unexpected = unexGen,
    ErrorGen.reason = reasonGen
  }

verifiedWithVanillaRaw :: (a -> Maybe String) -> Parsec a -> Parsec b
verifiedWithVanillaRaw = verifiedWithVanilla (const ErrorGen.RawItem)

{-
@since 0.2.3.0
-}
verifiedFail :: (a -> [String]) -> Parsec a -> Parsec b
verifiedFail msggen = verifiedWith $
  ErrorGen.specializedGen {
    ErrorGen.messages = msggen
  }

{-
@since 0.2.3.0
-}
verifiedUnexpected :: Parsec a -> Parsec b
verifiedUnexpected = verifiedWithVanillaRaw (const Nothing)

{-
@since 0.2.3.0
-}
verifiedExplain :: (a -> String) -> Parsec a -> Parsec b
verifiedExplain reasongen = verifiedWithVanillaRaw (Just . reasongen)

{-
@since 0.2.3.0
-}
preventWith :: ErrorGen a -> Parsec a -> Parsec ()
preventWith err p = amend (ErrorGen.asSelect err (withWidth (hide (atomic p)) <+> unit))

preventWithVanilla :: (a -> ErrorGen.UnexpectedItem) -> (a -> Maybe String) -> Parsec a -> Parsec ()
preventWithVanilla unexGen reasonGen = preventWith $
  ErrorGen.vanillaGen {
    ErrorGen.unexpected = unexGen,
    ErrorGen.reason = reasonGen
  }

preventWithVanillaRaw :: (a -> Maybe String) -> Parsec a -> Parsec ()
preventWithVanillaRaw = preventWithVanilla (const ErrorGen.RawItem)

{-
@since 0.2.3.0
-}
preventativeFail :: (a -> [String]) -> Parsec a -> Parsec ()
preventativeFail msggen = preventWith $
  ErrorGen.specializedGen {
    ErrorGen.messages = msggen
  }

{-
@since 0.2.3.0
-}
preventativeExplain :: (a -> String) -> Parsec a -> Parsec ()
preventativeExplain reasongen = preventWithVanillaRaw (Just . reasongen)
