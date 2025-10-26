{-# LANGUAGE TypeFamilies
           , FlexibleContexts #-}
{-|
Adapted from https://github.com/j-mie6/ParsleyHaskell/tree/master.
-}
module Shared.BenchmarkUtils (
  parsecParse,
  megaParse,
  gigaParse,
  attoParse,
  flatParse,
  string,
  text,
  bytestring,
  lazyBytestring,
  benchmarkFiles,
  condensedMain,
  Benchmark
) where

import Test.Tasty.Bench         

import Control.DeepSeq        (NFData)
import Control.Monad.Identity (Identity)
import Data.Text              (Text)
import Data.ByteString        (ByteString)
import Text.Parsec qualified  as Parsec
import Text.Megaparsec qualified as Megaparsec
import Text.Gigaparsec qualified as Gig
import Data.Attoparsec.Text qualified as Attoparsec 
import FlatParse.Basic qualified as FlatParse
import Data.Text.IO qualified
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Shared.Parsec.Extended qualified
import Shared.Megaparsec.Extended qualified
import Data.Either (fromRight)

parsecParse :: Parsec.Stream s Identity Char => Shared.Parsec.Extended.Parser s a -> s -> Maybe a
parsecParse p = either (const Nothing) Just  . Parsec.parse p ""

megaParse :: (Megaparsec.Stream s, Megaparsec.Token s ~ Char) => Shared.Megaparsec.Extended.Parser s a -> s -> Maybe a
megaParse = Megaparsec.parseMaybe

gigaParse :: Gig.Parsec a -> String -> Maybe a
gigaParse p xs = Gig.result (const Nothing) Just (Gig.parse @String p xs)

attoParse :: Attoparsec.Parser a -> Text -> Maybe a
attoParse p = rightToMaybe . Attoparsec.parseOnly p
  where
    rightToMaybe x = fromRight Nothing (Just <$> x)

flatParse :: FlatParse.Parser e a -> ByteString -> Maybe a
flatParse p bs = case FlatParse.runParser p bs of
  FlatParse.OK x _ -> Just x
  _ -> Nothing

string :: FilePath -> IO String
string = readFile
text :: FilePath -> IO Text
text = Data.Text.IO.readFile
bytestring :: FilePath -> IO ByteString
bytestring = Data.ByteString.readFile
lazyBytestring :: FilePath -> IO Data.ByteString.Lazy.ByteString
lazyBytestring = Data.ByteString.Lazy.readFile

benchmarkFiles :: (NFData a, NFData rep) => [FilePath] -> (FilePath -> IO rep) -> String -> (rep -> Maybe a) -> Benchmark
benchmarkFiles filenames load lib parser = env (traverse load filenames) (bgroup lib . (tasks filenames))
  where
    tasks filenames inputs = foldr (\f ts n -> bench f (nf (foo . parser) (inputs !! n)) : ts (n+1)) (const []) filenames 0
    foo n = case n of
      Just x -> x
      Nothing -> error "bench fail"

condensedMain :: [Benchmark] -> IO ()
condensedMain = defaultMain
