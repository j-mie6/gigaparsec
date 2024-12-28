{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SmallPython.AST.ToPython (
    StringBuilder(..),
    sb,
    ToPython(..)
  ) where
import SmallPython.AST
import GHC.Exts ( IsString(..), IsList, IsList(..) )
import Data.List (intercalate, intersperse)
import qualified Data.List.NonEmpty as NE

newtype StringBuilder = StringBuilder { unStringBuilder :: (String -> String) }
  deriving newtype (Semigroup, Monoid)

instance Show StringBuilder where
  show :: StringBuilder -> String
  show (StringBuilder x) = x ""

buildString :: StringBuilder -> String
buildString (StringBuilder sb) = sb ""

sb :: String -> StringBuilder
sb x = StringBuilder (x ++)

_to :: ToPython a => a -> StringBuilder
_to = toPythonStringBuilder

instance IsString StringBuilder where
  fromString :: String -> StringBuilder
  fromString = sb

class ToPython a where
  toPythonStringBuilder :: a -> StringBuilder

  toPythonString :: a -> String
  toPythonString = buildString . toPythonStringBuilder

instance ToPython AtomNumber where
  toPythonStringBuilder :: AtomNumber -> StringBuilder
  toPythonStringBuilder = sb . \case
    AtomInt x -> show x
    AtomDouble x -> show x

instance ToPython Atom where
  toPythonStringBuilder :: Atom -> StringBuilder
  toPythonStringBuilder = \case
    AtomVar x -> sb x
    AtomNumber x -> _to x
    AtomString x -> sb x
    AtomChar x -> sb [x]

instance ToPython BinOpSymbol where
  toPythonStringBuilder :: BinOpSymbol -> StringBuilder
  toPythonStringBuilder op = case op of
    BinPlus -> "+"
    BinMinus -> "-"
    BinMult -> "*"
    BinDiv -> "/"
    BinFloorDiv -> "//"
    BinExponent -> "**"

instance ToPython BinOp where
  toPythonStringBuilder :: BinOp -> StringBuilder
  toPythonStringBuilder (_, op) = _to op

instance ToPython UnaryOpSymbol where
  toPythonStringBuilder :: UnaryOpSymbol -> StringBuilder
  toPythonStringBuilder op = case op of
    UnaryPlus -> "+"
    UnaryMinus -> "-"

instance ToPython UnaryOp where
  toPythonStringBuilder :: UnaryOp -> StringBuilder
  toPythonStringBuilder (_, op) = _to op


class Precedence a where
  precedence :: a -> PrecedenceLevel

data PrecedenceLevel = 
    PrecAdd
  | PrecMul
  | PrecExp
  | PrecAtom
  deriving (Eq, Ord, Bounded)

instance Precedence UnaryOp where
  precedence :: UnaryOp -> PrecedenceLevel
  precedence (_, op) = case op of
    UnaryPlus  -> PrecAdd
    UnaryMinus -> PrecAdd

instance Precedence BinOp where
  precedence :: BinOp -> PrecedenceLevel
  precedence (_, op) = case op of
    BinPlus  -> PrecAdd
    BinMinus -> PrecAdd
    BinDiv -> PrecMul
    BinFloorDiv -> PrecMul
    BinMult -> PrecMul
    BinExponent -> PrecExp

instance Precedence Expr where
  precedence :: Expr -> PrecedenceLevel
  precedence t = case t of
    ExprAtom {} -> PrecAtom
    ExprBin _ op _ _ -> precedence op
    ExprUnary _ op _ -> precedence op
    ExprFunctionCall {} -> PrecAtom

instance ToPython Expr where
  toPythonStringBuilder :: Expr -> StringBuilder
  toPythonStringBuilder t = withPrecedence minBound t
    where
    withPrecedence :: PrecedenceLevel -> Expr -> StringBuilder
    withPrecedence outer t = 
      let inner = precedence t 
      in  paren outer inner $ case t of
        (ExprAtom _ a) -> _to a
        (ExprBin _ op t u) -> 
          mconcat [withPrecedence inner t, " ", _to op , " ", withPrecedence inner u]
        (ExprUnary _ op t) -> _to op <> withPrecedence inner t
        (ExprFunctionCall _ f as) -> _to f <> "(" <> mconcat (intersperse ", " (map _to as)) <> ")"

    paren outer inner x
      | outer > inner = "(" <> x <> ")"
      | otherwise     = x


instance ToPython Params where
  toPythonStringBuilder :: Params -> StringBuilder
  toPythonStringBuilder = mconcat . intersperse ", " . map (\(x, y) -> maybe (sb x) ((sb x <>) . ("=" <>) . _to) y)

instance ToPython ExprLHS where
  toPythonStringBuilder :: ExprLHS -> StringBuilder
  toPythonStringBuilder = \case
    LHSIdent x -> sb x

instance ToPython AssignType where
  toPythonStringBuilder :: AssignType -> StringBuilder
  toPythonStringBuilder = \case
    AssignSimple -> "="
    AssignPlus   -> "+="

_toStmt :: Stat -> [StringBuilder]
_toStmt = map ("\n" <>) . go
  where
    go = \case
      StatFunctionDef _ f ps sts ->
        mconcat ["def ", sb f, "(", _to ps, "):"] :
        concat (NE.toList (fmap ((map (indString <>) . go)) sts))

      StatPass _ -> ["pass"]
      StatAssign _ asgn lhs t -> [mconcat [_to lhs, " ", _to asgn, " ", _to t]]
      StatReturn _ e -> ["return " <> _to e]
      StatExp _ e -> [_to e]
    indString :: StringBuilder
    indString = "    "
instance ToPython Stat where
  toPythonStringBuilder :: Stat -> StringBuilder
  toPythonStringBuilder = mconcat . _toStmt

instance ToPython Program where
  toPythonStringBuilder :: Program -> StringBuilder
  toPythonStringBuilder (Program sts) = mconcat $ map toPythonStringBuilder sts


-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- module SmallPython.AST.ToPython (
--     StringBuilder(..),
--     sb,
--     ToPython(..)
--   ) where
-- import SmallPython.AST
-- import GHC.Exts (IsString (..))
-- import Data.List (intercalate, intersperse)

-- newtype StringBuilder = StringBuilder (String -> String)
--   deriving newtype (Semigroup, Monoid)


-- sb :: String -> StringBuilder
-- sb x = StringBuilder (x ++)

-- _to :: ToPython a => a -> StringBuilder
-- _to = toPythonStringBuilderWithIndent

-- instance IsString StringBuilder where
--   fromString :: String -> StringBuilder
--   fromString = sb

-- class ToPython a where
--   toPythonStringBuilderWithIndent :: Word -> Word -> a -> StringBuilder

--   toPythonStringBuilder :: a -> StringBuilder
--   toPythonStringBuilder = toPythonStringBuilderWithIndent 0

-- instance ToPython AtomNumber where
--   toPythonStringBuilderWithIndent :: Word -> AtomNumber -> StringBuilder
--   toPythonStringBuilderWithIndent = sb . \case 
--     AtomInt x -> show x
--     AtomDouble x -> show x

-- instance ToPython Atom where
--   toPythonStringBuilderWithIndent :: Word -> Atom -> StringBuilder
--   toPythonStringBuilderWithIndent = sb . \case
--     AtomVar x -> x 
--     AtomNumber x -> show x 
--     AtomString x -> x 
--     AtomChar x -> [x] 

-- instance ToPython BinOpSymbol where
--   toPythonStringBuilderWithIndent :: Word -> BinOpSymbol -> StringBuilder
--   toPythonStringBuilderWithIndent op = case op of
--     BinPlus -> "+"
--     BinMinus -> "-"
--     BinMult -> "*"
--     BinDiv -> "/"
--     BinFloorDiv -> "//"
--     BinExponent -> "**"

-- instance ToPython BinOp where
--   toPythonStringBuilderWithIndent :: Word -> BinOp -> StringBuilder
--   toPythonStringBuilderWithIndent (_, op) = _to op

-- instance ToPython UnaryOpSymbol where
--   toPythonStringBuilderWithIndent :: Word -> UnaryOpSymbol -> StringBuilder
--   toPythonStringBuilderWithIndent op = case op of
--     UnaryPlus -> "+"
--     UnaryMinus -> "-"

-- instance ToPython UnaryOp where
--   toPythonStringBuilderWithIndent :: Word -> UnaryOp -> StringBuilder
--   toPythonStringBuilderWithIndent (_, op) = _to op

-- instance ToPython Expr where
--   toPythonStringBuilderWithIndent :: Word -> Expr -> StringBuilder
--   toPythonStringBuilderWithIndent t = case t of 
--     (ExprAtom _ a) -> _to a
--     (ExprBin _ op t u) -> mconcat ["(", _to t, ") ", _to op ," (", _to u, ")"]
--     (ExprUnary _ op t@(ExprBin {})) -> mconcat [_to op ," (", _to t, ")"]
--     (ExprUnary _ op t) -> _to op <> _to t
--     (ExprFunctionCall _ f as) -> _to f <> "(" <> mconcat (intersperse ", " (map _to as)) <> ")"


-- instance ToPython Params where
--   toPythonStringBuilderWithIndent :: Word -> Params -> StringBuilder
--   toPythonStringBuilderWithIndent = mconcat . map (\(x, y) -> maybe (sb x) ((sb x <>) . ("=" <>) . _to) y) 

-- instance ToPython ExprLHS where
--   toPythonStringBuilderWithIndent :: Word -> ExprLHS -> StringBuilder
--   toPythonStringBuilderWithIndent = \case
--     LHSIdent x -> sb x

-- instance ToPython AssignType where
--   toPythonStringBuilderWithIndent :: Word -> AssignType -> StringBuilder
--   toPythonStringBuilderWithIndent = \case
--     AssignSimple -> "="
--     AssignPlus   -> "+="

-- instance ToPython Stat where
--   toPythonStringBuilderWithIndent :: Word -> Stat -> StringBuilder
--   toPythonStringBuilderWithIndent = \case
--     StatFunctionDef _ f ps sts -> 
--       let sts' = intersperse "\n "
--       in  mconcat [
--         "def ", sb f, "(", _to ps, "):\n"
--       , mconcat (fmap (("" <>) . _to) sts)
--       ]
--     StatPass _ -> "pass"
--     StatAssign _ asgn lhs t -> mconcat [_to lhs, " ", _to asgn, " ", _to t]
--     StatReturn _ e -> "return " <> _to e
--     StatExp _ e -> _to e
