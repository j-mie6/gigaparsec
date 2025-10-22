{-# LANGUAGE
      TemplateHaskell
  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-|
Many of the parts here are taken from FlatParse.Examples.BasicLambda
-}
module JavascriptBench.FlatParse.Parser where

import Data.ByteString qualified as B hiding (unpack)
import Data.ByteString.Char8 qualified as B

import FlatParse.Basic hiding (Parser)
import FlatParse.Basic qualified as FP
import FlatParse.Common.Parser (PureMode)

import Shared.FlatParse.Utils 

import JavascriptBench.Shared
import Language.Haskell.TH (Code, Q, unsafeCodeCoerce)
import Data.Functor (($>))
import JavascriptBench.FlatParse.TH
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (digitToInt)
import Text.Read (readMaybe)


javascript :: Parser JSProgram
javascript = whitespace *> many element <* eof

stmt :: Parser JSStm
stmt = undefined
element :: Parser JSElement
element = undefined
compound :: Parser JSCompoundStm
compound = undefined

varsOrExprs :: Parser (Either [JSVar] JSExpr)
varsOrExprs = undefined
variable :: Parser JSVar
variable = undefined

parensExpr :: Parser JSExpr
parensExpr = undefined
optExpr :: Parser (Maybe JSExpr)
optExpr = undefined
expr :: Parser JSExpr
expr = undefined
asgn :: Parser JSExpr'
asgn = undefined
condExpr :: Parser JSExpr'
condExpr = undefined
expr' :: Parser JSExpr'
expr' = undefined
memOrCon :: Parser JSUnary
memOrCon = undefined
con :: Parser JSCons
con = undefined
conCall :: Parser JSCons
conCall = undefined
member :: Parser JSMember
member = undefined

primaryExpr :: Parser JSAtom
primaryExpr = (JSParens <$> parens expr)
          <|> (JSArray <$> brackets (commaSep asgn))
          <|> (JSId <$> identifier)
          <|> (either JSInt JSFloat <$> naturalOrFloat)
          <|> (JSString <$> stringLiteral)
          <|> (JSTrue <$  $$(keyword "true"))
          <|> (JSFalse <$ $$(keyword "false"))
          <|> (JSNull <$  $$(keyword "null"))
          <|> (JSThis <$  $$(keyword "this"))

identifier :: Parser String
identifier = B.unpack <$> (byteStringOf $
  withSpan (identStart *> skipMany identLetter) (\_ -> fails . isKeyword))

isKeyword :: Span -> Parser ()
isKeyword span = inSpan span do
  $$(switchFromSet jsKeywords (\_ -> [|| pure @Parser () ||]))
  eof

-------------------------------------------------------------------------------
-- Numbers

naturalOrFloat :: Parser (Either Int Double)
naturalOrFloat = natFloat <* whitespace

natFloat :: Parser (Either Int Double)
natFloat = $(char '0') *> zeroNumFloat
  <|> (optional decimal >>= decFloat)

decFloat :: Maybe Int -> Parser (Either Int Double)
decFloat n = do
  fract <- optional fraction
  exp <- optional exponent'
  mkNum n fract exp
  where
    mkNum :: Maybe Int -> Maybe String -> Maybe String -> Parser (Either Int Double)
    mkNum Nothing Nothing Nothing = failed
    mkNum (Just n) Nothing Nothing = pure $ Left n
    mkNum n fract exp = case readMaybe (show (fromMaybe 0 n) ++ (fromMaybe "" fract) ++ (fromMaybe "" exp)) of
      Nothing -> failed
      Just d -> pure $ Right d


-- A number starting w a zero could be a hex/oct literal or a floating decimal
zeroNumFloat :: Parser (Either Int Double)
zeroNumFloat = (Left <$> (hexadecimal <|> octal))
            <|> decFloat (Just 0)
            <|> pure (Left 0)

-------------------------------------------------------------------------------
-- Floats

fraction :: Parser [Char]
fraction = ('.' :) <$> ($$(tokenChar '.')
        *> some (inRange '0' '9'))

inRange :: Char -> Char -> (Parser Char)
inRange lo hi = satisfy (\c -> c >= lo && c <= hi)

exponent' :: Parser [Char]
exponent' = ('e' :) <$> ($$(tokenChar 'E') <|> $$(tokenChar 'e')
          *> ((
                  ((:) <$> $$(oneOf2 '+' '-')) 
              <|> pure id
              )
            <*> (show <$> decimal)))


-------------------------------------------------------------------------------
-- Integers

hexadecimal :: Parser Int
hexadecimal = $$(oneOf2 'x' 'X') *> anyAsciiHexInt

decimal :: Parser Int
decimal = anyAsciiDecimalInt

octalDigit :: Char -> Bool
octalDigit c = (c >= '0' && c <= '7')

octal :: Parser Int
octal = $$(oneOf2 'o' 'O') *> number 8 (satisfy octalDigit)

number :: Int -> Parser Char -> Parser Int
number base = pfoldl1 (\x d -> base * x + digitToInt d) 0




-------------------------------------------------------------------------------
-- Tokens


dot :: Parser ()
dot = $$(symbol  '.')
semi :: Parser ()
semi = $$(symbol ';')
comma :: Parser ()
comma = $$(symbol  ',')

parens :: Parser a -> Parser a
parens = between $$(symbol '(') $$(symbol ')')
brackets :: Parser a -> Parser a
brackets = between $$(symbol '[') $$(symbol ']')
braces :: Parser a -> Parser a
braces = between $$(symbol '{') $$(symbol '}')

between :: Parser a -> Parser c -> Parser b -> Parser b
between start end middle = start *> middle <* end


commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = 
  withOption p (\x -> (x:) <$> many (sep *> p)) (pure [])

-------------------------------------------------------------------------------
-- Strings and Chars

stringLiteral :: Parser String
stringLiteral = catMaybes 
  <$> between 
        $$(tokenChar '\"') 
        $$(tokenChar '\"') 
        (many stringChar)
  <* whitespace


stringChar :: Parser (Maybe Char)
stringChar = (Just <$> satisfy jsStringLetter) <|> stringEscape

stringEscape :: Parser (Maybe Char)
stringEscape = $(switch [| case _ of 
    "\\&" -> pure @Parser Nothing
    "\\ " -> (skipMany space) $> Nothing
    "\\"  -> Just <$> escapeCode
  |])

-- Parses any permissible escape code that can appear after a '\'
escapeCode :: Parser Char
escapeCode = 
  $$(switchFromSet 
      jsEscapeCodes 
      (\es -> 
        let y = jsEscapeCharFromString es 
        in [|| pure @Parser y ||]
      )
  )
