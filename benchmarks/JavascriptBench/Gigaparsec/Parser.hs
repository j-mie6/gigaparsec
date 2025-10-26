
{-# LANGUAGE 
      Safe
    , TypeFamilies
    , ScopedTypeVariables
    , FlexibleContexts
    , OverloadedLists
    , FlexibleInstances
#-}
{-|
Adapted from https://github.com/j-mie6/ParsleyHaskell/tree/master.
-}
module JavascriptBench.Gigaparsec.Parser where

import Control.Applicative (liftA3, Alternative)
import Data.Char (isSpace, isUpper, digitToInt)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Read (readMaybe)

import Text.Gigaparsec
import Text.Gigaparsec.Char hiding (space, spaces, whitespace)
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Expr.Chain
import Text.Gigaparsec.Expr

import JavascriptBench.Shared


javascript :: Parsec JSProgram
javascript = whitespace *> many element <* eof

element :: Parsec JSElement
element = keyword "function" *> liftA3 JSFunction identifier (parens (commaSep identifier)) compound
      <|> JSStm <$> stmt


compound :: Parsec JSCompoundStm
compound = braces (many stmt)

-- Parse a javascript statement
stmt :: Parsec JSStm
stmt = semi $> JSSemi
    <|> keyword "if" *> liftA3 JSIf parensExpr stmt (option (keyword "else" *> stmt))
    <|> keyword "while" *> liftA2 JSWhile parensExpr stmt
    <|> (keyword "for" *> parens
            (atomic (liftA2 JSForIn varsOrExprs (keyword "in" *> expr))
        <|> liftA3 JSFor (option varsOrExprs <* semi) (optExpr <* semi) optExpr)
        <*> stmt)
    <|> keyword "break" $> JSBreak
    <|> keyword "continue" $> JSContinue
    <|> keyword "with" *> liftA2 JSWith parensExpr stmt
    <|> keyword "return" *> (JSReturn <$> optExpr)
    <|> JSBlock <$> compound
    <|> JSNaked <$> varsOrExprs
varsOrExprs :: Parsec (Either [JSVar] JSExpr)
varsOrExprs = (keyword "var" *> commaSep1 variable) <+> expr
variable :: Parsec JSVar
variable = liftA2 JSVar identifier (option (symbol '=' *> asgn))
parensExpr :: Parsec JSExpr
parensExpr = parens expr
optExpr :: Parsec (Maybe JSExpr)
optExpr = option expr
expr :: Parsec JSExpr
expr = commaSep1 asgn
asgn :: Parsec JSExpr'
asgn = chainl1 condExpr (symbol '=' $> JSAsgn)
condExpr :: Parsec JSExpr'
condExpr = liftA2 jsCondExprBuild expr' (option ((symbol '?' *> asgn) <~> (symbol ':' *> asgn)))
expr' :: Parsec JSExpr'
expr' = precedence $ 
      Atom (JSUnary <$> memOrCon)
  >+
      ops Prefix [
          operator "--" $> jsDec
        , operator "++" $> jsInc
        , operator "-" $> jsNeg
        , operator "+" $> jsPlus
        , operator "~" $> jsBitNeg
        , operator "!" $> jsNot
        ]
  >+  ops Postfix [ 
          operator "--" $> jsDec
        , operator "++" $> jsInc 
        ]
  >+  ops InfixL  [ 
          operator "*" $> JSMul
        , operator "/" $> JSDiv
        , operator "%" $> JSMod 
        ]
  >+  ops InfixL  [ 
          operator "+" $> JSAdd
        , operator "-" $> JSSub 
        ]
  >+  ops InfixL  [ 
          operator "<<" $> JSShl
        , operator ">>" $> JSShr 
        ]
  >+  ops InfixL  [ 
          operator "<=" $> JSLe
        , operator "<" $> JSLt
        , operator ">=" $> JSGe
        , operator ">" $> JSGt 
        ]
  >+  ops InfixL  [ 
          operator "==" $> JSEq
        , operator "!=" $> JSNe 
        ]
  >+  ops InfixL  [ atomic (operator "&") $> JSBitAnd ]
  >+  ops InfixL  [ operator "^" $> JSBitXor ]
  >+  ops InfixL  [ atomic (operator "|") $> JSBitOr ]
  >+  ops InfixL  [ operator "&&" $> JSAnd ]
  >+  ops InfixL  [ operator "||" $> JSOr ]
  

memOrCon :: Parsec JSUnary
memOrCon = keyword "delete" *> (JSDel <$> member)
        <|> keyword "new" *> (JSCons <$> con)
        <|> JSMember <$> member
con :: Parsec JSCons
con = liftA2 JSQual (keyword "this" $> "this") (dot *> conCall) <|> conCall
conCall :: Parsec JSCons
conCall = identifier <**>
            (dot *> (flip JSQual <$> conCall)
          <|> flip JSConCall <$> parens (commaSep asgn)
          <|> pure (\name -> JSConCall name []))
member :: Parsec JSMember
member = primaryExpr <**>
            (flip JSCall <$> parens (commaSep asgn)
          <|> flip JSIndex <$> brackets expr
          <|> dot *> ((flip JSAccess) <$> member)
          <|> pure JSPrimExp)
primaryExpr :: Parsec JSAtom
primaryExpr = JSParens <$> parens expr
          <|> JSArray <$> brackets (commaSep asgn)
          <|> JSId <$> identifier
          <|> either JSInt JSFloat <$> naturalOrFloat
          <|> JSString <$> stringLiteral
          <|> JSTrue <$ keyword "true"
          <|> JSFalse <$ keyword "false"
          <|> JSNull <$ keyword "null"
          <|> JSThis <$ keyword "this"

-- Token Parsers
space :: Parsec ()
space = void (satisfy isSpace)
whitespace :: Parsec ()
whitespace = skipMany (spaces <|> oneLineComment <|> multiLineComment)
keyword :: String -> Parsec ()
keyword s = atomic (string s *> notIdentLetter) *> whitespace
operator :: String -> Parsec ()
operator op = atomic (string op *> notOpLetter) *> whitespace
identifier :: Parsec String
identifier = atomic (filterS jsUnreservedName (identStart <:> many identLetter)) <* whitespace
naturalOrFloat :: Parsec (Either Int Double)
naturalOrFloat = natFloat <* whitespace

-- Nonsense to deal with floats and ints
natFloat :: Parsec (Either Int Double)
natFloat = char '0' *> zeroNumFloat <|> decimalFloat

zeroNumFloat :: Parsec (Either Int Double)
zeroNumFloat = Left <$> (hexadecimal <|> octal)
            <|> decimalFloat
            <|> (fromMaybeS empty (fractFloat <*> pure 0))
            <|> pure (Left 0)

decimalFloat :: Parsec (Either Int Double)
decimalFloat = fromMaybeS empty (decimal <**> (option' (Just . Left) fractFloat))

fractFloat :: Parsec (Int -> Maybe (Either Int Double))
fractFloat = f <$> fractExponent
  where
    f g x = fmap Right (g x)

-- Parse the optional float and/or fraction.
-- The `Int` input is the digits before the decimal point.
fractExponent :: Parsec (Int -> Maybe Double)
fractExponent = f <$> fraction <*> option' "" exponent'
            <|> f <$> pure "" <*> exponent'
  where
    f fract exp n = readMaybe (show n ++ fract ++ exp)

-- Parse the stuff after a 'dot' in a float
fraction :: Parsec [Char]
fraction = ('.' :) <$> (char '.'
        *> some (oneOf ['0'..'9']))

-- Parse the exponent part of a float
exponent' :: Parsec [Char]
exponent' = ('e' :) <$> (oneOf (Set.fromList "eE")
          *> ((((:) <$> oneOf (Set.fromList "+-")) <|> pure id)
          <*> (show <$> decimal)))

decimal :: Parsec Int
decimal = number 10 (oneOf ['0'..'9'])

hexadecimal :: Parsec Int
hexadecimal = oneOf (Set.fromList "xX") *> number 16 (oneOf (['a'..'f'] <> ['A'..'F'] <> ['0'..'9']))

octal :: Parsec Int
octal = oneOf (Set.fromList "oO") *> number 8 (oneOf ['0'..'7'])

number :: Int -> Parsec Char -> Parsec Int
number base = somel (\x d -> base * x + digitToInt d) 0

stringLiteral :: Parsec String
stringLiteral = catMaybes <$> between (token "\"") (token "\"") (many stringChar) <* whitespace

between :: Parsec a -> Parsec b -> Parsec c -> Parsec c
between start end middle = start *> middle <* end

symbol :: Char -> Parsec Char
symbol c = atomic (char c) <* whitespace
parens :: Parsec a -> Parsec a
parens = between (symbol '(') (symbol ')')
brackets :: Parsec a -> Parsec a
brackets = between (symbol '[') (symbol ']')
braces :: Parsec a -> Parsec a
braces = between (symbol '{') (symbol '}')
dot :: Parsec Char
dot = symbol '.'
semi :: Parsec Char
semi = symbol ';'
comma :: Parsec Char
comma = symbol ','
commaSep :: Parsec a -> Parsec [a]
commaSep p = sepBy p comma
commaSep1 :: Parsec a -> Parsec [a]
commaSep1 p = sepBy1 p comma

-- Let bindings
spaces :: Parsec ()
spaces = skipSome space

oneLineComment :: Parsec ()
oneLineComment = void (token "//" *> skipMany (satisfy (/= '\n')))

multiLineComment :: Parsec ()
multiLineComment =
  let inComment = void (token "*/")
              <|> skipSome (noneOf (Set.fromList "/*")) *> inComment
              <|> oneOf (Set.fromList "/*") *> inComment
  in token "/*" *> inComment

identStart :: Parsec Char
identStart = satisfy jsIdentStart
identLetter :: Parsec Char
identLetter = satisfy jsIdentLetter

notIdentLetter :: Parsec ()
notIdentLetter = notFollowedBy identLetter
notOpLetter :: Parsec ()
notOpLetter = notFollowedBy (oneOf (Set.fromList "+-*/=<>!~&|.%^"))

-- | Try @p@, return @x@ if @p@ fails w/o consuming input.
option' :: Alternative f => a -> f a -> f a
option' x p = p <|> pure x

-- The set of possible characters starting an escape character sequence.
escChrs :: Set Char
escChrs = Set.fromList "abfntv\\\"'0123456789xo^ABCDEFGHLNRSUV"

-- | Parse a single character in a js string
stringChar :: Parsec (Maybe Char)
stringChar = Just <$> satisfy jsStringLetter <|> stringEscape

-- | Parse a single escape character sequence in a js string
stringEscape :: Parsec (Maybe Char)
stringEscape = token "\\" *> (token "&" $> Nothing
                          <|> spaces *> token "\\" $> Nothing
                          <|> Just <$> escapeCode)

-- | A token is (a possibly multi-char) something which is either totally parsed
-- or not at all, the latter not consuming input.
token :: String -> Parsec String
token = atomic . string


-- Parses any permissible escape code that can appear after a '\'
escapeCode :: Parsec Char
escapeCode = oneOf escChrs >>= escCode
-- match escChrs (oneOf escChrs) escCode empty
  where
    -- Given the starting character, how to parse the rest of the escape sequence.
    escCode :: Char -> Parsec Char
    escCode 'a' = pure ('\a')
    escCode 'b' = pure ('\b')
    escCode 'f' = pure ('\f')
    escCode 'n' = pure ('\n')
    escCode 't' = pure ('\t')
    escCode 'v' = pure ('\v')
    escCode '\\' = pure ('\\')
    escCode '"' = pure ('"')
    escCode '\'' = pure ('\'')
    escCode '^' = (\c -> toEnum (fromEnum c - fromEnum 'A' + 1)) <$> satisfy isUpper
    escCode 'A' = token "CK" $> ('\ACK')
    escCode 'B' = token "S" $> ('\BS') <|> token "EL" $> ('\BEL')
    escCode 'C' = token "R" $> ('\CR') <|> token "AN" $> ('\CAN')
    escCode 'D' = token "C" *> (token "1" $> ('\DC1')
                          <|> token "2" $> ('\DC2')
                          <|> token "3" $> ('\DC3')
                          <|> token "4" $> ('\DC4'))
            <|> token "EL" $> ('\DEL')
            <|> token "LE" $> ('\DLE')
    escCode 'E' = token "M" $> ('\EM')
            <|> token "T" *> (token "X" $> ('\ETX')
                          <|> token "B" $> ('\ETB'))
            <|> token "SC" $> ('\ESC')
            <|> token "OT" $> ('\EOT')
            <|> token "NQ" $> ('\ENQ')
    escCode 'F' = token "F" $> ('\FF') <|> token "S" $> ('\FS')
    escCode 'G' = token "S" $> ('\GS')
    escCode 'H' = token "T" $> ('\HT')
    escCode 'L' = token "F" $> ('\LF')
    escCode 'N' = token "UL" $> ('\NUL') <|> token "AK" $> ('\NAK')
    escCode 'R' = token "S" $> ('\RS')
    escCode 'S' = token "O" *> option' (('\SO')) (token "H" $> ('\SOH'))
            <|> token "I" $> ('\SI')
            <|> token "P" $> ('\SP')
            <|> token "TX" $> ('\STX')
            <|> token "YN" $> ('\SYN')
            <|> token "UB" $> ('\SUB')
    escCode 'U' = token "S" $> ('\US')
    escCode 'V' = token "T" $> ('\VT')
    -- TODO numeric
    escCode _ = empty--error "numeric escape codes not supported"