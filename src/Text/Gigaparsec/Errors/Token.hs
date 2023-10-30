{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Errors.Token (module Text.Gigaparsec.Errors.Token) where

data Token = Raw !String | Named !String {-# UNPACK #-} !Word
