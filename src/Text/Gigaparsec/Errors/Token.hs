{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Errors.Token (module Text.Gigaparsec.Errors.Token) where

type Token :: *
data Token = Raw !String | Named !String {-# UNPACK #-} !Word

span :: Token -> Word
span (Raw cs) = fromIntegral (length cs)
span (Named _ w) = w
