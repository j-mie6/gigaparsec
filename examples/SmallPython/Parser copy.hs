{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


{-|
This module defines the parser for (small)python.
-}
module SmallPython.Parser where
import Prelude hiding (fail)

import Text.Gigaparsec hiding (some)
import Text.Gigaparsec.Combinator.NonEmpty


import SmallPython.Lexer
import SmallPython.AST
import Text.Gigaparsec.Position (pos)
import Text.Gigaparsec.Combinator (sepBy)
import Text.Gigaparsec.Char (endOfLine)
import Text.Gigaparsec.Expr.Chain (chainr1)
import Text.Gigaparsec.Expr.Chain (chainr)
import Text.Gigaparsec.Expr.Infix (infixr1)
import Text.Gigaparsec.Expr qualified as Expr
import Text.Gigaparsec.Expr hiding (Atom)
import Text.Gigaparsec.Internal qualified as Internal



import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Control.Monad.Reader (ReaderT, Reader, MonadReader (local), MonadTrans (lift))
import Text.Gigaparsec.State (Ref)
import Data.IORef (IORef)
import Text.Gigaparsec.Token.Indentation (IndentLevel)
import Text.Gigaparsec.Token.Indentation (myIndentSome)
import Text.Gigaparsec.Token.Indentation

---------------------------------------------------------------------------------------------------
-- Atoms

number :: Parsec Atom
number = AtomNumber . AtomInt <$> integer

-- perhaps more efficient to change the return type depending on if followed by "("?
variable :: Parsec Atom
variable = (AtomVar <$> ident) <* notFollowedBy "("

atom :: Parsec Expr
atom = mkExprAtom (
      variable
  <|> number)


---------------------------------------------------------------------------------------------------
-- Expressions

paren :: Parsec a -> Parsec a
paren p = "(" *> p <* ")"

parenChangeWs :: Parsec a -> Parsec a
parenChangeWs p = paren (alter isParenWhiteSpace (whiteSpace *> p))

args :: Parsec [Expr]
args = parenChangeWs (sepBy expr ",")

functionCall :: Parsec Expr
functionCall = mkFunctionCall expr args

expr :: Parsec Expr
expr = precedence $
    Expr.Atom (atom <|> parenChangeWs expr <|> functionCall)
  >+  sops InfixR [ 
        exprBinApp BinExponent "**"
      ]
  >+  sops InfixL [ 
        exprBinApp BinMult "*"
      , exprBinApp BinDiv "/"
      , exprBinApp BinFloorDiv "//" 
      ]
  >+  sops InfixL [
        exprBinApp BinMult "*"
      ]

  where
    exprBinApp 
      :: BinOpSymbol 
      -> Parsec ()
      -> Parsec (Expr -> Expr -> Expr)
    exprBinApp op symbol = mkExprBin <*> (mkBinOp op <* symbol)

params :: Parsec Params
params = parenChangeWs (sepBy param ",")
  where
    param =
      (,) <$> ident 
          <*> (Just <$> ("=" *> expr) <|> return Nothing)

exprLHS :: Parsec ExprLHS
exprLHS = LHSIdent <$> ident

assignment :: Parsec AssignType
assignment = 
      ("="  $> AssignSimple)
  <|> ("+=" $> AssignPlus)

-- stat :: MIndentLevel -> Parsec Stat
-- stat currIndent = 
--       (endOfLine *> whiteSpace *> stat currIndent) -- Allow empty lines
--   <|> stats currIndent

stats :: MIndentLevel -> Parsec Stats
stats currIndent = 
    chainr 
      NonEmpty.singleton 
      (stat currIndent)
      (endOfLine $> NonEmpty.cons)

stats1 :: MIndentLevel -> Parsec Stats1
stats1 currIndent = 
  myIndentSome whiteSpace currIndent stat
    -- infixr1 
    --   NonEmpty.singleton 
    --   (stat currIndent)
    --   (endOfLine $> NonEmpty.cons)


stat :: MIndentLevel -> Parsec Stat
stat currIndent = 
      mkFunctionDef ("def" *> ident) params 
        (":" *> endOfLine *> 
          (myIndentSome whiteSpace currIndent stats1)
        )
  <|> mkStatAssign exprLHS assignment expr
  <|> mkReturn ("return" *> expr)

program :: Parsec [Stat]
program = fully $ many (stat Nothing)


-- singleStat :: IndentParser Stat
-- singleStat = 
--       ((lift endOfLine) *> singleStat)
--   <|> (mkFunctionDef ("def" *> ident) params (":" *> endOfLine *> indentBlock stat))

-- indentBlock :: Parsec a -> Parsec a
-- indentBlock p = _

-- newtype Indentation = Indentation Word

-- incIndent :: Indentation -> Indentation
-- incIndent (Indentation n) = Indentation (n + 1)

-- type IndentParser = ReaderT Indentation Parsec

-- class IsParser p where

-- indentRef :: IORef Indentation
-- indentRef = _

-- indentAfter :: IndentParser a -> IndentParser b -> IndentParser (a, b)
-- indentAfter p q = do
--   x <- p
--   y <- local incIndent q
--   return (x, y)
-- -- indentAfter p = Internal.Parsec $ \st ok err -> _

-- -- indentBlock :: 

-- type IndentParser2 a = Reader Indentation (Parsec a)

-- singleStat2 :: IndentParser2 Stat
-- singleStat2 = 
--       (endOfLine *> singleStat2)
--   <|> (mkFunctionDef ("def" *> ident) params (":" *> endOfLine *> indentBlock2 stat))


-- indentAfter2 :: IndentParser2 a -> IndentParser2 b -> IndentParser2 (a, b) 
-- indentAfter2 p q = do
--   x <- p
--   y <- local incIndent q
--   return $ (,) <$> x <*> y

-- indentBlock2 :: IndentParser2 a -> IndentParser2 a
-- indentBlock2 = local incIndent
