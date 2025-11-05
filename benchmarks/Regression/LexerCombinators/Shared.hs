module Regression.LexerCombinators.Shared where

import Text.Gigaparsec.Token.Descriptions qualified as D
import Data.Char (isAlpha, isAlphaNum, isSpace, readLitChar)
import Text.Gigaparsec.Token.Lexer qualified as L
import Text.Gigaparsec (Parsec)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map



identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'

identLetter :: Char -> Bool
identLetter c = isAlphaNum c || c == '_'

lineCommentStart :: String
lineCommentStart = "//"

multiLineCommentStart :: String
multiLineCommentStart = "/*"

multiLineCommentEnd :: String
multiLineCommentEnd = "*/"


unreservedName :: String -> Bool
unreservedName s = not (Set.member s keywords)

keywords :: Set String
keywords = Set.fromList [
  "true", "false", "if", "else",
  "for", "while", "break", "continue", "in",
  "function", "var", "new", "delete",
  "this", "null", "return", "with"
  ]

stringLetter :: Char -> Bool
stringLetter c = (c /= '"') && (c /= '\\') && (c > '\026')

escapeLiteralsSet :: Set Char
escapeLiteralsSet = Set.fromList escapeLiterals

escapeLiterals :: [Char]
escapeLiterals = [
    '\\', '"', '\'', '^'
  ]

-- 'a', 'b' , 'f', 'n' , 't', 'v'

escapeSequencesMap :: Map String Char
escapeSequencesMap = Map.fromList $ zip escapeSequences asLiterals
  where
    asLiterals = map fst $ concatMap (readLitChar . ('\\' :)) escapeSequences

escapeSequences :: [String]
escapeSequences = [
    "a", "b" , "f", "n" , "t", "v"
  , "ACK", "BS" , "BEL", "CR" , "CAN", "DC1", "DC2", "DC3"
  , "DC4", "DEL", "DLE", "EM" , "ETX", "ETB", "ESC", "EOT"
  , "ENQ", "FF" , "FS" , "GS" , "HT" , "LF" , "NUL", "NAK"
  , "RS" , "SO" , "SOH", "SI" , "SP" , "STX", "SYN", "SUB"
  , "US" , "VT"
  ]

escapeLiteralsAll :: [String]
escapeLiteralsAll = (map (:"") escapeLiterals) ++ escapeSequences
