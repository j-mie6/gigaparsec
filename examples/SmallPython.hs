{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}


module Main (main) where

import SmallPython.AST
import SmallPython.Parser
import SmallPython.Lexer

import Text.Pretty.Simple

import Text.Gigaparsec
import SmallPython.AST.ToPython (ToPython(..))


main :: IO ()
main = return ()


parseFile :: FilePath -> IO ()
parseFile fp = do
  prog <- parseFromFile @String program fp
  case prog of
    Failure err -> putStrLn err
    Success p -> pPrint p

parseFilePretty :: FilePath -> IO ()
parseFilePretty fp = do
  prog <- parseFromFile @String program fp
  case prog of
    Failure err -> putStrLn err
    Success p -> putStrLn $ toPythonString p

