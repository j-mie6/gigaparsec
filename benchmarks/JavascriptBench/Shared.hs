{-|
Taken from https://github.com/j-mie6/ParsleyHaskell/tree/master.
-}
{-# LANGUAGE 
  StandaloneDeriving, 
  DeriveAnyClass, 
  DeriveGeneric 
#-}
{-|
Taken from: https://github.com/j-mie6/ParsleyHaskell/blob/8a9e679fc148e7532ccb8100263a6c9f19f2f42a/parsley/benchmarks/JavascriptBench/Shared.hs
-}
module JavascriptBench.Shared where

import Control.DeepSeq (NFData(..), rwhnf, deepseq)
import GHC.Generics    (Generic)
import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper, isDigit, digitToInt, readLitChar)
import Data.Set (fromList, member, Set)
import Data.Map (Map)
import Data.Map qualified as Map

type JSProgram = [JSElement]

type JSCompoundStm = [JSStm]

type JSExpr = [JSExpr']

data JSElement =
    JSFunction !String ![String] !JSCompoundStm
  | JSStm !JSStm
  deriving Show

data JSStm =
    JSSemi
  | JSIf !JSExpr !JSStm !(Maybe JSStm)
  | JSWhile !JSExpr !JSStm
  | JSFor !(Maybe (Either [JSVar] JSExpr)) !(Maybe JSExpr) !(Maybe JSExpr) !JSStm
  | JSForIn !(Either [JSVar] JSExpr) !JSExpr !JSStm
  | JSBreak
  | JSContinue
  | JSWith !JSExpr !JSStm
  | JSReturn !(Maybe JSExpr)
  | JSBlock !JSCompoundStm
  | JSNaked !(Either [JSVar] JSExpr)
  deriving Show

data JSVar = JSVar !String !(Maybe JSExpr')
  deriving Show

data JSExpr' =
    JSAsgn   !JSExpr' !JSExpr'
  | JSCond   !JSExpr' !JSExpr' !JSExpr'
  | JSOr     !JSExpr' !JSExpr'
  | JSAnd    !JSExpr' !JSExpr'
  | JSBitOr  !JSExpr' !JSExpr'
  | JSBitXor !JSExpr' !JSExpr'
  | JSBitAnd !JSExpr' !JSExpr'
  | JSEq     !JSExpr' !JSExpr'
  | JSNe     !JSExpr' !JSExpr'
  | JSLt     !JSExpr' !JSExpr'
  | JSGt     !JSExpr' !JSExpr'
  | JSLe     !JSExpr' !JSExpr'
  | JSGe     !JSExpr' !JSExpr'
  | JSShl    !JSExpr' !JSExpr'
  | JSShr    !JSExpr' !JSExpr'
  | JSAdd    !JSExpr' !JSExpr'
  | JSSub    !JSExpr' !JSExpr'
  | JSMul    !JSExpr' !JSExpr'
  | JSDiv    !JSExpr' !JSExpr'
  | JSMod    !JSExpr' !JSExpr'
  | JSUnary  !JSUnary
  deriving Show

data JSUnary =
    JSPlus   !JSUnary
  | JSNeg    !JSUnary
  | JSBitNeg !JSUnary
  | JSNot    !JSUnary
  | JSInc    !JSUnary
  | JSDec    !JSUnary
  | JSNew    !JSCons
  | JSDel    !JSMember
  | JSMember !JSMember
  | JSCons   !JSCons
  deriving Show
jsPlus (JSUnary u)   = JSUnary (JSPlus u)
jsNeg (JSUnary u)    = JSUnary (JSNeg u)
jsBitNeg (JSUnary u) = JSUnary (JSBitNeg u)
jsNot (JSUnary u)    = JSUnary (JSNot u)
jsInc (JSUnary u)    = JSUnary (JSInc u)
jsDec (JSUnary u)    = JSUnary (JSDec u)
data JSMember = JSPrimExp !JSAtom
              | JSAccess  !JSAtom !JSMember
              | JSIndex   !JSAtom !JSExpr
              | JSCall    !JSAtom !JSExpr deriving Show
data JSCons = JSQual !String !JSCons
            | JSConCall !String !JSExpr deriving Show
data JSAtom =
    JSParens !JSExpr
  | JSArray  !JSExpr
  | JSId     !String
  | JSInt    !Int
  | JSFloat  !Double
  | JSString !String
  | JSTemplateLit !String
  | JSTrue
  | JSFalse
  | JSNull
  | JSThis
  deriving Show

deriving instance Generic JSElement
deriving instance Generic JSStm
deriving instance Generic JSVar
deriving instance Generic JSExpr'
deriving instance Generic JSUnary
deriving instance Generic JSMember
deriving instance Generic JSCons
deriving instance Generic JSAtom

deriving instance NFData JSElement
deriving instance NFData JSStm
deriving instance NFData JSVar
deriving instance NFData JSExpr'
deriving instance NFData JSUnary
deriving instance NFData JSMember
deriving instance NFData JSCons
deriving instance NFData JSAtom


jsCondExprBuild :: JSExpr' -> Maybe (JSExpr', JSExpr') -> JSExpr'
jsCondExprBuild c (Just (t, e)) = JSCond c t e
jsCondExprBuild c Nothing       = c

jsIdentStart :: Char -> Bool
jsIdentStart c = isAlpha c || c == '_'

jsIdentLetter :: Char -> Bool
jsIdentLetter c = isAlphaNum c || c == '_'

jsUnreservedName :: String -> Bool
jsUnreservedName s = not (member s jsKeywords)


jsKeywords :: Set String
jsKeywords = fromList [
  "true", "false", "if", "else",
  "for", "while", "break", "continue", "in",
  "function", "var", "new", "delete",
  "this", "null", "return", "with"
  ]

jsStringLetter :: Char -> Bool
jsStringLetter c = (c /= '"') && (c /= '\\') && (c > '\026')

jsEscapeCodes :: Set String
jsEscapeCodes = fromList [
    "a", "b", "f", "n", "t", "v", "\\", "\"", "'", "^", "ACK"
  , "BS", "BEL"
  , "CR", "CAN"
  , "DC1", "DC2", "DC3", "DC4", "DEL", "DLE"
  , "EM", "ETX", "ETB", "ETX", "ESC", "EOT", "ENQ"
  , "FF", "FS"
  , "GS", "HT", "LF", "NUL", "NAK", "RS"
  , "SO", "SOH", "SI", "SP", "STX", "SYN", "SUB"
  , "US", "VT"
  ]

jsEscapeSingleCharLiterals :: Set Char
jsEscapeSingleCharLiterals = fromList [
    'a', 'b' , 'f', 'n' , 't'
  , 'v', '\\', '"', '\'', '^'
  ]

jsEscapeMultiCharSet :: Set String
jsEscapeMultiCharSet = fromList [
    "ACK", "BS" , "BEL", "CR" , "CAN", "DC1", "DC2", "DC3"
  , "DC4", "DEL", "DLE", "EM" , "ETX", "ETB", "ESC", "EOT"
  , "ENQ", "FF" , "FS" , "GS" , "HT" , "LF" , "NUL", "NAK"
  , "RS" , "SO" , "SOH", "SI" , "SP" , "STX", "SYN", "SUB"
  , "US" , "VT"
  ]

jsEscapeCharFromString :: String -> Char
jsEscapeCharFromString "^" = '^'
jsEscapeCharFromString xs = case readLitChar $ ('\\' :) xs of
  (ec, _): _ -> ec
  [] -> error $ "jsEscapeCharFromString failed on string" ++ xs

jsEscapeMultiCharSequenceMap :: Map String Char
jsEscapeMultiCharSequenceMap = Map.fromList $ zip sequences asLiterals
  where
    asLiterals = map fst $ concatMap (readLitChar . ('\\' :)) sequences

    sequences :: [String]
    sequences = [
        "ACK", "BS" , "BEL", "CR" , "CAN", "DC1", "DC2", "DC3"
      , "DC4", "DEL", "DLE", "EM" , "ETX", "ETB", "ESC", "EOT"
      , "ENQ", "FF" , "FS" , "GS" , "HT" , "LF" , "NUL", "NAK"
      , "RS" , "SO" , "SOH", "SI" , "SP" , "STX", "SYN", "SUB"
      , "US" , "VT"
      ]