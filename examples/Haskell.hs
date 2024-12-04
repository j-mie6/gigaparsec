module Main where

import Haskell.Parser
import Haskell.AST
import Haskell.AST.ToHaskell (ToHaskell(..))
import Haskell.Lexer

import GHC.IO (throwIO)

import Text.Gigaparsec

main :: IO ()
main = pure ()

-- parseFileShow :: FilePath -> IO ()
-- parseFileShow x = print =<< parseFile x



-- parseFile :: FilePath -> IO Program
-- parseFile f = do
--   x <- parseFromFile @String program f
--   case x of
--     Success p -> return p
--     Failure e -> throwIO (userError e)

parseString :: Parsec a -> String -> Result String a
parseString p = parse (fully p)

-- parseFilePretty :: FilePath -> IO ()
-- parseFilePretty x = putStrLn . pretty =<< parseFile x
parsePretty :: ToHaskell a => Parsec a -> String -> IO ()
parsePretty p x = case parseString p x of
  Success y -> putStrLn $ toHaskell y
  Failure e -> putStrLn $ "Error: " ++ e

-- parseProg :: String -> Result String Program
-- parseProg = parseString program

parseExpr :: String -> Result String Expr
parseExpr = parseString expr


-- parsePrettyExpr :: String -> IO ()
-- parsePrettyExpr = putStrLn . parsePretty expr

-- parsePrettyProg :: String -> IO ()
-- parsePrettyProg = putStrLn . parsePretty program
