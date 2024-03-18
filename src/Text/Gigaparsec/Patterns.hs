{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-|
Module      : Text.Gigaparsec.Patterns
Description : Template Haskell generators to help with patterns
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module is currently experimental, and may have bugs depending on the version
of Haskell, or the extensions enabled. Please report any issues to the maintainers.

@since 0.2.2.0
-}
module Text.Gigaparsec.Patterns (deriveLiftedConstructors, deriveDeferredConstructors) where

import Prelude (
    Bool(True, False), String, Int, Maybe(Just, Nothing), Eq((==), (/=)),
    fmap, map, concat, (.), traverse, sequence, foldr1, length, (-), return, (++),
    fail, ($), unwords, maybe, otherwise, id, reverse, show, flip, takeWhile, (+)
  )

import Text.Gigaparsec (Parsec, (<**>), (<*>), pure)
import Text.Gigaparsec.Position (Pos, pos)
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Maybe (isJust, isNothing)
import Language.Haskell.TH (
    Q, Exp, Name, Dec,
    Type (ForallT, AppT, ArrowT, StarT, ConT),
    Info (DataConI, PatSynI), TyVarBndr (KindedTV, PlainTV),
    sigD, funD, clause, varP, normalB, varE, reify, mkName, newName,
    isExtEnabled, Extension (KindSignatures),
    forallT, conE, lamE
  )
#if __GLASGOW_HASKELL__ >= 902
import Language.Haskell.TH (Type(MulArrowT))
#endif

{-|
This function is used to automatically generate /Lifted Constructors/, which are
one of the patterns in /"Design Patterns for Parser Combinators/". It is provided
with a prefix, which is used to denote an application of the constructor, and
then a list of "ticked" constructors to generate lifted constructors for. This
means adding a single @'@ in front of the constructor name. For example:

> {-# LANGUAGE TemplateHaskell #-}
> data Foo a = Foo a | Bar Int String
> $(deriveLiftedConstructors "mk" ['Foo, 'Bar])

Will generate two lifted constructors of the shape:

> mkFoo :: Parsec a -> Parsec (Foo a)
> mkBar :: Parsec Int -> Parsec String -> Parsec (Foo a)

Furthermore, if one of the arguments to the constructor has type `Text.Gigaparsec.Position.Pos`,
the @pos@ combinator will be applied automatically, and this will not be apparent in the signature
of the generated constructor.

@since 0.2.2.0

Pattern synonyms can be used to set type parameters to `Text.Gigaparsec.Position.Pos`:

> {-# LANGUAGE PatternSynonyms #-}
> pattern PosFoo :: Pos -> Foo Pos
> pattern PosFoo p = Foo p
> $(deriveLiftedConstructors "mk" ['PosFoo])

This will generate a lifted constructor of the shape:

> mkPosFoo :: Parsec (Foo Pos)

The @pos@ combinator will be applied automatically.

@since 0.2.6.0
-}
deriveLiftedConstructors :: String -- ^ The prefix to be added to generated names
                         -> [Name] -- ^ The list of "ticked" constructors to generate for
                         -> Q [Dec]
deriveLiftedConstructors prefix = fmap concat . traverse deriveCon
  where
    deriveCon :: Name -> Q [Dec]
    deriveCon con = do
      (con', ty, func, posFound, n) <- extractMeta True prefix (funcType . map parserOf) con
      args <- replicateM n (newName "x")
      sequence [ sigD con' ty
               , funD con' [clause (map varP args)
                   (normalB (posAp posFound (applyArgs [e|pure $func|] args))) []]
               ]

    applyArgs :: Q Exp -> [Name] -> Q Exp
    applyArgs = foldl' (\rest arg -> [e|$rest <*> $(varE arg)|])


{-|
This function is used to automatically generate /Deferred Constructors/, which are
one of the patterns in /"Design Patterns for Parser Combinators/". It is provided
with a prefix, which is used to denote an application of the constructor, and
then a list of "ticked" constructors to generate deferred constructors for. This
means adding a single @'@ in front of the constructor name. For example:

> {-# LANGUAGE TemplateHaskell #-}
> data Foo a = Foo a | Bar Int String
> $(deriveDeferredConstructors "mk" ['Foo, 'Bar])

Will generate two deferred constructors of the shape:

> mkFoo :: Parsec (a -> Foo a)
> mkBar :: Parsec (Int -> String -> Foo a)

Furthermore, if one of the arguments to the constructor has type `Text.Gigaparsec.Position.Pos`,
the @pos@ combinator will be applied automatically, and this will not be apparent in the signature
of the generated constructor.

@since 0.2.2.0

Pattern synonyms can be used to set type parameters to `Text.Gigaparsec.Position.Pos`:

> {-# LANGUAGE PatternSynonyms #-}
> pattern PosFoo :: Pos -> Foo Pos
> pattern PosFoo p = Foo p
> $(deriveLiftedConstructors "mk" ['PosFoo])

This will generate a lifted constructor of the shape:

> mkPosFoo :: Parsec (Foo Pos)

The @pos@ combinator will be applied automatically.

@since 0.2.6.0
-}
deriveDeferredConstructors :: String -- ^ The prefix to be added to generated names
                           -> [Name] -- ^ The list of "ticked" constructors to generate for
                           -> Q [Dec]
deriveDeferredConstructors prefix = fmap concat . traverse deriveCon
  where
    deriveCon :: Name -> Q [Dec]
    deriveCon con = do
      (con', ty, func, posFound, _) <- extractMeta False prefix (parserOf . funcType) con
      sequence [ sigD con' ty
               , funD con' [clause [] (normalB (posAp posFound [e|pure $func|])) []]
               ]

posAp :: Bool -> Q Exp -> Q Exp
posAp True  p = [e| pos <**> $p |]
posAp False p = p

funcType :: [Q Type] -> Q Type
funcType = foldr1 (\ty rest -> [t| $ty -> $rest |])

parserOf :: Q Type -> Q Type
parserOf ty = [t| Parsec $ty |]

extractConType :: Info -> Maybe Type
extractConType (DataConI _ ty _) = Just ty
extractConType (PatSynI _ ty) = Just ty
extractConType _ = Nothing

extractMeta :: Bool -> String -> ([Q Type] -> Q Type) -> Name
          -> Q (Name, Q Type, Q Exp, Bool, Int)
extractMeta posLast prefix buildType con = do
  Just ty <- fmap extractConType $ reify con
  (forAll, tys) <- splitFun ty
  posIdx <- findPosIdx con tys
  let tys' = maybeApply deleteAt posIdx tys
  let nargs = length tys' - 1
  let con' = mkName (prefix ++ pretty con)
  let func = buildLiftedLambda posLast con nargs posIdx
  return (con', forAll (buildType (map return tys')), func, isJust posIdx, nargs)

splitFun :: Type -> Q (Q Type -> Q Type, [Type])
splitFun (ForallT bndrs ctx ty) = do
  kindSigs <- isExtEnabled KindSignatures
  let bndrs' = if kindSigs then bndrs else map sanitiseStarT bndrs
  (forallT', ty') <- splitFun ty
  return (forallT bndrs' (pure ctx) . forallT', ty')
splitFun ty                     = return (id, splitFun' ty)

splitFun' :: Type -> [Type]
splitFun' (AppT (AppT ArrowT a) b)             = a : splitFun' b -- regular function type
#if __GLASGOW_HASKELL__ >= 902
splitFun' (AppT (AppT (AppT MulArrowT _) a) b) = a : splitFun' b -- linear function type
#endif
splitFun' ty                                   = [ty]

-- When KindSignatures is off, the default (a :: *) that TH generates is broken!
#if __GLASGOW_HASKELL__ >= 900
sanitiseStarT :: TyVarBndr flag -> TyVarBndr flag
sanitiseStarT (KindedTV ty flag StarT) = PlainTV ty flag
sanitiseStarT ty = ty
#else
sanitiseStarT :: TyVarBndr -> TyVarBndr
sanitiseStarT (KindedTV ty StarT) = PlainTV ty
sanitiseStarT ty = ty
#endif

findPosIdx :: Name -> [Type] -> Q (Maybe Int)
findPosIdx con tys = case elemIndices (ConT ''Pos) tys of
     []    -> return Nothing
     [idx] -> return (Just idx)
     _     -> fail $ unwords -- more than 1 index, which is ambiguous
        ["constructor", pretty con, "has multiple occurrences of Text.Gigaparsec.Position.Pos"]

buildLiftedLambda :: Bool -> Name -> Int -> Maybe Int -> Q Exp
buildLiftedLambda posLast con nargs posIdx = do
  args <- replicateM nargs (newName "x")
  posArg <- newName "pos"
  let pargs = if | isNothing posIdx -> map varP args
                 | posLast          -> map varP args ++ [varP posArg]
                 | otherwise        -> varP posArg : map varP args
  let eargs = maybeApply (flip insertAt (varE posArg)) posIdx (map varE args)
  lamE pargs (foldl' (\acc arg -> [e|$acc $arg|]) (conE con) eargs)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = x : xs
insertAt n x (x' : xs) = x' : insertAt (n - 1) x xs
insertAt _ _ [] = []

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:xs)  = xs
deleteAt n (x:xs)  = x : deleteAt (n-1) xs
deleteAt _ []      = []

elemIndices :: forall a. Eq a => a -> [a] -> [Int]
elemIndices = go 0
  where go :: Int -> a -> [a] -> [Int]
        go _ _ [] = []
        go i y (x:xs)
          | x == y     = i : go (i + 1) y xs
          | otherwise  = go (i + 1) y xs

pretty :: Name -> String
pretty = reverse . takeWhile (/= '.') . reverse . show
