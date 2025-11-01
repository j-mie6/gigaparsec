module Regression.LexerCombinators.Shared where

import Text.Gigaparsec.Token.Descriptions qualified as D
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Text.Gigaparsec.Token.Lexer qualified as L
import Text.Gigaparsec (Parsec)
import Data.Set (Set)
import Data.Set qualified as Set



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
