{-# LANGUAGE TypeFamilies
           , FlexibleContexts #-}
{-|
Adapted from https://github.com/j-mie6/ParsleyHaskell/tree/master.
-}
module Shared.BenchmarkUtils where

import Gauge.Main             (Benchmark, bgroup, bench, nf, defaultMainWith, env)
import Gauge.Main.Options     (Config(displayMode), defaultConfig, DisplayMode(Condensed))
import Control.DeepSeq        (NFData)
import Control.Monad.Identity (Identity)
import Data.Text              (Text)
import Data.ByteString        (ByteString)
import Text.Parsec qualified  as Parsec
import Text.Megaparsec qualified as Megaparsec
import Text.Gigaparsec qualified as Gig
import Data.Attoparsec.Text qualified as Attoparsec 
import Data.Text.IO qualified
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Shared.Parsec.Extended qualified
import Shared.Megaparsec.Extended qualified

parsecParse :: Parsec.Stream s Identity Char => Shared.Parsec.Extended.Parser s a -> s -> Maybe a
parsecParse p = either (const Nothing) Just  . Parsec.parse p ""

megaParse :: (Megaparsec.Stream s, Megaparsec.Token s ~ Char) => Shared.Megaparsec.Extended.Parser s a -> s -> Maybe a
megaParse = Megaparsec.parseMaybe

gigaParse :: Gig.Parsec a -> String -> Maybe a
gigaParse p xs = Gig.result (const Nothing) Just (Gig.parse @String p xs)

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = Attoparsec.maybeResult . Attoparsec.parse p

string          :: FilePath -> IO String
string          = readFile
text            :: FilePath -> IO Text
text            = Data.Text.IO.readFile
bytestring      :: FilePath -> IO ByteString
bytestring      = Data.ByteString.readFile
lazy_bytestring :: FilePath -> IO Data.ByteString.Lazy.ByteString
lazy_bytestring = Data.ByteString.Lazy.readFile

benchmarkFiles :: (NFData a, NFData rep) => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> Maybe a) -> Benchmark
benchmarkFiles filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (nf parser (inputs !! n)) : ts (n+1)) (const []) filenames 0

condensedMain :: [Benchmark] -> IO ()
condensedMain = defaultMainWith (defaultConfig {displayMode = Condensed})
