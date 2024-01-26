{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Gauge (defaultMain, bench, nf)
import Text.Gigaparsec (Parsec, Result, parse, atomic, (<|>))
import Text.Gigaparsec.Char (string)
import Control.DeepSeq (NFData)

p :: Parsec String
p = atomic (string "hello wold") <|> atomic (string "hi") <|> string "hello world"

deriving anyclass instance (NFData a, NFData e) => NFData (Result e a)

main :: IO ()
main = defaultMain [
    bench "consumption" $ nf (parse @String p) "hello world"
  ]
