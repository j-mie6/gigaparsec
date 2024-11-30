{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Text.Gigaparsec.Internal.TH.TypeUtils
Description : Common Template Haskell Utils
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

Common utils for manipulating Template Haskell Type AST.

@since 0.2.2.0
-}
module Text.Gigaparsec.Internal.TH.TypeUtils (module Text.Gigaparsec.Internal.TH.TypeUtils) where

import Text.Gigaparsec.Internal.TH.VersionAgnostic

import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH (pprint)

-- When KindSignatures is off, the default (a :: *) that TH generates is broken!
sanitiseStarT :: TyVarBndr flag -> TyVarBndr flag
sanitiseStarT = recTyVarBndr mkPlainTV (\x y -> const (mkPlainTV x y))

-- | Remove stars from binder annotations when we don't have `KindSignatures` enabled.
sanitiseBndrStars :: [TyVarBndr flag] -> Q [TyVarBndr flag]
sanitiseBndrStars bndrs = do
  kindSigs <- isExtEnabled KindSignatures
  return (if kindSigs then bndrs else map sanitiseStarT bndrs)

sanitiseTypeStars :: Type -> Q Type
sanitiseTypeStars = cataType go
 where
  go :: TypeF (Q Type) -> Q Type
  go (ForallTF bnds ctx tp) =
    ForallT <$> mapM helpBnd bnds <*> sequence ctx <*> tp
  go (ForallVisTF bnds tp) = mkForallVisT <$> mapM helpBnd bnds <*> tp
  go e = embedType <$> sequence e

  helpBnd :: TyVarBndrF flag (Q Type) -> Q (TyVarBndr flag)
  helpBnd (PlainTVF n f) = return $ mkPlainTV n f
  helpBnd (KindedTVF n f ~_) = return $ mkPlainTV n f

unTyVarBndrF :: TyVarBndrF flag k -> (Name, flag, Maybe k)
unTyVarBndrF = recTyVarBndrF (,,Nothing) (\x y z -> (x, y, Just z))

reTyVarBndr :: Name -> flag -> Maybe Type -> TyVarBndr flag
reTyVarBndr n f mt = case mt of
  Nothing -> mkPlainTV n f
  Just t -> mkKindedTV n f t

getBndrFName :: TyVarBndrF flag k -> Name
getBndrFName = (\(a, _, _) -> a) . unTyVarBndrF

removeUnusedTVars :: Type -> Type
removeUnusedTVars = zygoType typeFreeVarsAlg go
 where
  go :: TypeF (Set Name, Type) -> Type
  go (ForallTF bnds ctx (tpNames, tp)) =
    let (ctxNames, ctx') = unzip ctx
        -- All names that *do* occur in the rest of the type/constraints
        allFreeNames = Set.unions (tpNames : ctxNames)
        (bnds', _) = foldr discardUnusedTVars ([], allFreeNames) bnds
     in ForallT bnds' ctx' tp
  go (ForallVisTF bnds (tpNames, tp)) =
    let (bnds', _) = foldr discardUnusedTVars ([], tpNames) bnds
     in ForallVisT bnds' tp
  go e = embedType (snd <$> e)

  discardUnusedTVars ::
    TyVarBndrF s (Set Name, Type) ->
    ([TyVarBndr s], Set Name) ->
    ([TyVarBndr s], Set Name)
  discardUnusedTVars bnd (bnds, names) =
    let (n, f, mk) = unTyVarBndrF bnd
     in if n `Set.member` names
          -- We keep n, and add any free vars in its kind (if it has one)
          then
            ( reTyVarBndr n f (snd <$> mk) : bnds
            , Set.union (maybe Set.empty fst mk) names
            )
          -- We discard n as it does not appear in subterms
          else (bnds, names)

typeFreeVarsAlg :: TypeF (Set Name) -> Set Name
typeFreeVarsAlg = go
 where
  go :: TypeF (Set Name) -> Set Name
  go (VarTF x) = Set.singleton x
  go (ForallTF bnds ctx tp) = handleBnds bnds (Set.unions $ (tp : ctx))
  go (ForallVisTF bnds tp) = handleBnds bnds tp
  go e = foldr Set.union Set.empty e

  handleBnds :: [TyVarBndrF flag (Set Name)] -> Set Name -> Set Name
  handleBnds bnds ns =
    let (as, ks) =
          bimap Set.fromList Set.unions $ unzip (map bndrFreeAndBoundNames bnds)
     in Set.difference (Set.union ks ns) as

  bndrFreeAndBoundNames :: TyVarBndrF flag (Set Name) -> (Name, Set Name)
  bndrFreeAndBoundNames (PlainTVF x _) = (x, Set.empty)
  bndrFreeAndBoundNames (KindedTVF x _ k) = (x, k)

typeFreeVars :: Type -> Set Name
typeFreeVars = cataType typeFreeVarsAlg

getRecordFields :: Info -> Q [(Name, Type)]
getRecordFields i = case i of
  TyConI (DataD _ _ _ _ cstrs _) -> concat <$> mapM getFieldNames cstrs
  TyConI (NewtypeD _ _ _ _ cstr _) -> getFieldNames cstr
  DataConI _ _ tname -> getRecordFields =<< reify tname
  info -> fail $ concat ["getRecordFields: given info is not for a record: `", pprint info, "`"]
  where
    getFieldNames :: Con -> Q [(Name, Type)]
    getFieldNames (RecC _ tps) = return $ map (\(nm, _, tp) -> (nm, tp)) tps
    getFieldNames c = fail ("getRecordFields: Constructor is not a record: " ++ pprint c)
