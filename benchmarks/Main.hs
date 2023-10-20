{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
module Main where

import Gauge (defaultMain, bench, nf)
import Text.Gigaparsec (Parsec, Result(..), parse, atomic, (<|>))
import Text.Gigaparsec.Char (string)
import Control.DeepSeq
import GHC.Generics

p :: Parsec String
p = atomic (string "hello wold") <|> atomic (string "hi") <|> string "hello world"

deriving instance Generic (Result a)
deriving instance NFData a => NFData (Result a)

main :: IO ()
main = defaultMain [
    bench "consumption" $ nf (parse p) "hello world"
  ]
