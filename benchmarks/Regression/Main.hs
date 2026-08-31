{-|
Adapted from https://github.com/j-mie6/ParsleyHaskell/tree/master.
-}
{-# LANGUAGE 
      TemplateHaskell,
      ScopedTypeVariables,
      StandaloneDeriving,
      DeriveAnyClass,
      DeriveGeneric,
      TypeFamilies,
      UnboxedTuples,
      TypeApplications 
#-}
module Main where
import Test.Tasty.Bench      (Benchmark, bgroup)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

import Shared.BenchmarkUtils

import Regression.LexerCombinators (lexerCombinators)

main :: IO ()
main = do
  condensedMain [lexerCombinators]

