module JavascriptBench.Gigaparsec.Configured.Parser where

import Control.Applicative (liftA3, Alternative)


import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Expr.Chain
import Text.Gigaparsec.Expr

import JavascriptBench.Shared
import JavascriptBench.Gigaparsec.Configured.Lexer


javascript :: Parsec JSProgram
javascript = whitespace *> many element <* eof

element :: Parsec JSElement
element = keyword "function" *> liftA3 JSFunction identifier (parens (sepBy identifier ",")) compound
      <|> JSStm <$> stmt


compound :: Parsec JSCompoundStm
compound = "{" *> (many stmt) <* "}"

stmt :: Parsec JSStm
stmt = ";" $> JSSemi
    <|> "if" *> (JSIf <$> parensExpr <*> stmt <*> option ("else" *> stmt))
    <|> "while" *> (JSWhile <$> parensExpr <*> stmt)
    <|> ("for" *> parens (
            atomic (JSForIn <$> varsOrExprs <*> ("in" *> expr))
        <|> (JSFor <$> (option varsOrExprs <* ";") <*> (optExpr <* ";") <*> optExpr)
        ) <*> stmt)
    <|> "break" $> JSBreak
    <|> "continue" $> JSContinue
    <|> "with" *> (JSWith <$> parensExpr <*> stmt)
    <|> "return" *> (JSReturn <$> optExpr)
    <|> JSBlock <$> ("{" *> many stmt <* "}")
    <|> JSNaked <$> varsOrExprs

varsOrExprs :: Parsec (Either [JSVar] JSExpr)
varsOrExprs = (keyword "var" *> sepBy1 variable ",") <+> expr
variable :: Parsec JSVar
variable = liftA2 JSVar identifier (option ("=" *> asgn))
parensExpr :: Parsec JSExpr
parensExpr = parens expr
optExpr :: Parsec (Maybe JSExpr)
optExpr = option expr

parens :: Parsec a -> Parsec a
parens p = "(" *> p <* ")"

expr :: Parsec JSExpr
expr = sepBy1 asgn ","

asgn :: Parsec JSExpr'
asgn = chainl1 condExpr ("=" $> JSAsgn)

condExpr :: Parsec JSExpr'
condExpr = liftA2 jsCondExprBuild expr' (option (("?" *> asgn) <~> (":" *> asgn)))

expr' :: Parsec JSExpr'
expr' = precedence $
      ops Prefix [
          "--" $> jsDec
        , "++" $> jsInc
        , "-" $> jsNeg
        , "+" $> jsPlus
        , "~" $> jsBitNeg
        , "!" $> jsNot
        ]
  +<  ops Postfix [
          "--" $> jsDec
        , "++" $> jsInc
        ]
  +<  ops InfixL  [
          "*" $> JSMul
        , "/" $> JSDiv
        , "%" $> JSMod
        ]
  +<  ops InfixL  [
          "+" $> JSAdd
        , "-" $> JSSub
        ]
  +<  ops InfixL  [
          "<<" $> JSShl
        , ">>" $> JSShr
        ]
  +<  ops InfixL  [
          "<=" $> JSLe
        , "<" $> JSLt
        , ">=" $> JSGe
        , ">" $> JSGt
        ]
  +<  ops InfixL  [
          "==" $> JSEq
        , "!=" $> JSNe
        ]
  +<  ops InfixL  [ atomic "&" $> JSBitAnd ]
  +<  ops InfixL  [ "^" $> JSBitXor ]
  +<  ops InfixL  [ atomic "|" $> JSBitOr ]
  +<  ops InfixL  [ "&&" $> JSAnd ]
  +<  ops InfixL  [ "||" $> JSOr ]
  +<  Atom (JSUnary <$> memOrCon)

memOrCon :: Parsec JSUnary
memOrCon =  "delete" *> (JSDel <$> member)
        <|> "new" *> (JSCons <$> con)
        <|> JSMember <$> member
con :: Parsec JSCons
con = liftA2 JSQual ("this" $> "this") ("." *> conCall) <|> conCall
conCall :: Parsec JSCons
conCall = identifier <**>
            ("." *> (flip JSQual <$> conCall)
          <|> flip JSConCall <$> parens (sepBy asgn ",")
          <|> pure (`JSConCall` []))
member :: Parsec JSMember
member = primaryExpr <**>
            (flip JSCall <$> parens (sepBy asgn ",")
          <|> flip JSIndex <$> ("[" *> expr <* "]")
          <|> "." *> (flip JSAccess <$> member)
          <|> pure JSPrimExp)
primaryExpr :: Parsec JSAtom
primaryExpr = JSParens <$> parens expr
          <|> JSArray <$> ("[" *> sepBy asgn "," <* "]")
          <|> JSId <$> identifier
          <|> either JSInt JSFloat <$> naturalOrFloat
          <|> JSString <$> string
          <|> JSTrue <$ keyword "true"
          <|> JSFalse <$ keyword "false"
          <|> JSNull <$ keyword "null"
          <|> JSThis <$ keyword "this"
