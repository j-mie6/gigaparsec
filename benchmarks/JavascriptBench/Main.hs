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

import JavascriptBench.Shared
import JavascriptBench.Parsec.Parser qualified
import JavascriptBench.Megaparsec.Parser qualified
import JavascriptBench.Attoparsec.Parser qualified
import JavascriptBench.Happy.Parser qualified
-- import JavascriptBench.Parsley.Parser qualified
import JavascriptBench.Gigaparsec.Parser qualified as Gig
import JavascriptBench.Gigaparsec.Configured.Parser qualified as GigCfg
import JavascriptBench.FlatParse.Parser qualified

main :: IO ()
main = do
  condensedMain [javascript]

javascript :: Benchmark
javascript =
  let jsTest :: NFData rep => (FilePath -> IO rep) -> String -> (rep -> Maybe JSProgram) -> Benchmark
      jsTest = benchmarkFiles [
          "benchmarks/inputs/javascript/fibonacci.js"
        , "benchmarks/inputs/javascript/heapsort.js"
        , "benchmarks/inputs/javascript/game.js"
        , "benchmarks/inputs/javascript/big.js"
        ]
  in bgroup "Javascript" [
        jsTest string     "Gigaparsec (String)"  (gigaParse Gig.javascript)
      , jsTest string     "Gigaparsec Cfgd (String)"  (gigaParse GigCfg.javascript)
      , jsTest bytestring "FlatParse"           (flatParse JavascriptBench.FlatParse.Parser.javascript)
      , jsTest text       "Attoparsec"                 (attoParse JavascriptBench.Attoparsec.Parser.javascript)
      , jsTest string     "Happy"                (JavascriptBench.Happy.Parser.runParser JavascriptBench.Happy.Parser.javascript)
      , jsTest string     "Parsec (String)"      (parsecParse JavascriptBench.Parsec.Parser.javascript)
      , jsTest text       "Parsec (Text)"        (parsecParse JavascriptBench.Parsec.Parser.javascript)
      , jsTest string     "Megaparsec (String)"        (megaParse JavascriptBench.Megaparsec.Parser.javascript)
      , jsTest text       "Megaparsec (Text)"          (megaParse JavascriptBench.Megaparsec.Parser.javascript)
      ]
