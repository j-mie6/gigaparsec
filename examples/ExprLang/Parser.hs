{-# LANGUAGE OverloadedStrings #-}
module ExprLang.Parser where

import qualified Text.Gigaparsec.Token.Descriptions  as Desc
import qualified Text.Gigaparsec.Token.Lexer         as Lexer

import           Data.Char                           (generalCategory, isAlpha,
                                                      isAlphaNum, isSpace)
import qualified Data.Char                           as Char (GeneralCategory (Space))
import           Data.Int                            (Int64)
import           Data.List.NonEmpty                  (NonEmpty)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.String                         (IsString (fromString))
import           Data.Word                           (Word64)
import           Text.Gigaparsec                     (Parsec, Result(..),
                                                      parseFromFile, (<|>), ($>))
import           Text.Gigaparsec.Char                (newline, string, satisfy)
import           Text.Gigaparsec.Combinator           (endBy, optional)
import           Text.Gigaparsec.Expr                (Fixity (InfixL),
                                                      Prec (Atom), precedence,
                                                      sops, (>+))
import           Text.Gigaparsec.Patterns            (deriveDeferredConstructors,
                                                      deriveLiftedConstructors)
import           Text.Gigaparsec.Token.Errors        (Bits)
import           Text.Gigaparsec.Token.Patterns      (overloadedStrings)
import Data.List (intercalate, intersperse)


import ExprLang.AST
import ExprLang.Lexer

exprParen :: Parsec Expr
exprParen = "(" *> alter isSpace expr <* ")"

anyWhiteSpace :: Parsec ()
anyWhiteSpace = satisfy isSpace $> ()

atom :: Parsec Expr
atom = ExprAtom <$> (
        AtomInt <$> integer
    <|> AtomVar <$> identifier)
  <|> exprParen


expr :: Parsec Expr
expr = precedence $
      Atom atom 
  >+  sops InfixL [ mkExprBin <*> (mkOpMult <* "*"), mkExprBin <*> mkOpDiv <* "/" ]
  >+  sops InfixL [ mkExprBin <*> (mkOpAdd <* "+"),  mkExprBin <*> mkOpSubtract <* "-" ]
  

stat :: Parsec Stat
stat = mkStatAssign identifier ("=" *> expr)

return_ :: Parsec Return
return_ = mkReturn ("return" *> expr <* optional anyWhiteSpace)

program :: Parsec Program
program = fully $
  mkProgram (endBy stat newline) return_
