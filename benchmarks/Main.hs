{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Gauge (defaultMain, bench, nf)
import Text.Gigaparsec (Parsec, Result(Success, Failure), parse, atomic, (<|>))
import Text.Gigaparsec.Char (string)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

p :: Parsec String
p = atomic (string "hello wold") <|> atomic (string "hi") <|> string "hello world"

deriving stock instance Generic (Result a)
deriving anyclass instance NFData a => NFData (Result a)

main :: IO ()
main = defaultMain [
    bench "consumption" $ nf (parse p) "hello world"
  ]
