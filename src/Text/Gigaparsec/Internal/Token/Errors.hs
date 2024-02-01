{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Internal.Token.Errors (module Text.Gigaparsec.Internal.Token.Errors) where

import Data.Set (Set)
import Data.Map (Map)

type LabelWithExplainConfig :: *
data LabelWithExplainConfig = LENotConfigured
                            | LELabel !(Set String)
                            | LEReason !String
                            | LEHidden
                            | LELabelAndReason !(Set String) !String

type LabelConfig :: *
data LabelConfig = LNotConfigured
                 | LLabel !(Set String)
                 | LHidden

type ExplainConfig :: *
data ExplainConfig = ENotConfigured
                   | EReason !String

type FilterConfig :: * -> *
data FilterConfig a = VSBasicFilter
                    | VSSpecializedFilter (a -> [String])
                    | VSUnexpected (a -> String)
                    | VSBecause (a -> String)
                    | VSUnexpectedBecause (a -> String) (a -> String)

type VanillaFilterConfig :: * -> *
data VanillaFilterConfig a = VBasicFilter
                           | VUnexpected (a -> String)
                           | VBecause (a -> String)
                           | VUnexpectedBecause (a -> String) (a -> String)

type SpecializedFilterConfig :: * -> *
data SpecializedFilterConfig a = SBasicFilter
                               | SSpecializedFilter (a -> [String])

type VerifiedBadChars :: *
data VerifiedBadChars = BadCharsFail !(Map Char [String])
                      | BadCharsReason !(Map Char String)
                      | BadCharsUnverified
