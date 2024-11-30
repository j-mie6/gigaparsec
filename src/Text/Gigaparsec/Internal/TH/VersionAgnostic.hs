{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell, CPP, PatternSynonyms, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-|
Mostly some utils for dealing with TH in a version-agnostic way.
-}
module Text.Gigaparsec.Internal.TH.VersionAgnostic (
  -- * `TH.Type` Smart Constructors
  mkVarT,
  mkConT,
  mkPromotedT,
  mkSigT,
  mkAppT,
  mkForallT,
  -- *** Version ≥ 2.11
  mkInfixT,
  mkUInfixT,
  mkParensT,
  -- *** Version ≥ 2.15
  mkAppKindT,
  mkImplicitParamT,
  -- *** Version ≥ 2.16
  mkForallVisT,
  -- *** Version ≥ 2.19
  mkPromotedInfixT,
  mkPromotedUInfixT,
  -- * TyVarBndr
  -- ** Constructors and Recursors
  TyVarBndr,
  mkPlainTV,
  mkKindedTV,
  recTyVarBndr,
  -- ** Base Functor
  TyVarBndrF,
  pattern PlainTVF,
  pattern KindedTVF,
  recTyVarBndrF,
  projectBnd,
  embedBnd,
  -- * `TH.Type` Base Functor
  TypeF,
  -- ** View Patterns
  pattern ForallTF,
  pattern ForallVisTF, 
  pattern AppTF, 
  pattern AppKindTF, 
  pattern SigTF, 
  pattern InfixTF, 
  pattern UInfixTF, 
  pattern PromotedInfixTF, 
  pattern PromotedUInfixTF, 
  pattern ImplicitParamTF,
  pattern AtomicF, 
  pattern ParensTF,
  pattern VarTF,
  -- ** Recursion and Corecursion
  projectType,
  embedType,
  cataType,
  zygoType,
  -- * Template Haskell select re-exports
  module TH,
  module TH.Lib,
  TH.pprint,
#if !(MIN_VERSION_template_haskell(2,17,0))
  Quote(..),
  DocLoc(..),
  pattern MulArrowT,
  getDoc,
  putDoc,
#endif
  ) where

#if (MIN_VERSION_template_haskell(2,17,0))
import Language.Haskell.TH.Syntax hiding (TyVarBndr(..), Specificity)
import Language.Haskell.TH qualified as TH hiding (TyVarBndr(..), Specificity)
import Language.Haskell.TH.Syntax qualified as TH hiding (TyVarBndr(..), Specificity)
import Language.Haskell.TH.Syntax qualified as THAll
import Language.Haskell.TH.Lib as TH.Lib
#else
import Data.IORef (atomicModifyIORef')
import Language.Haskell.TH.Syntax hiding (TyVarBndr(..), Specificity, newName)
import Language.Haskell.TH qualified as TH hiding (TyVarBndr(..), Specificity)
import Language.Haskell.TH.Syntax qualified as TH hiding (TyVarBndr(..), Specificity)
import Language.Haskell.TH.Syntax qualified as THAll
import Language.Haskell.TH.Lib as TH.Lib
#endif


import Control.Applicative (liftA2)

import GHC.Generics (Generic)
import Data.Kind (Constraint)
import Data.Bitraversable (bisequence)


---------------------------------------------------------------------------------------------------
-- TyVarBndr Base Functor

-- use this defn so that TypeF doesn't need to have two recursion params
type TyVarBndrF :: * -> * -> *
type TyVarBndrF flag k = Either (Name, flag) (Name, flag, k)


{-# COMPLETE PlainTVF, KindedTVF #-}


pattern PlainTVF :: Name -> flag -> TyVarBndrF flag k
pattern KindedTVF :: Name -> flag -> k -> TyVarBndrF flag k
pattern PlainTVF n f = Left (n, f)
pattern KindedTVF n f knd = Right (n, f, knd)


recTyVarBndrF 
  :: (Name -> flag -> a) -- The `PlainTV` case
  -> (Name -> flag -> k -> a) -- The `KindedTV` case
  -> TyVarBndrF flag k
  -> a
recTyVarBndrF f _ (PlainTVF nm ~flag) = f nm flag
recTyVarBndrF _ g (KindedTVF nm ~flag k) = g nm flag k

{-| Unrolls one step of recursion on `TyVarBndr`.

Projects a `TyVarBndr` onto its base functor.
-}
projectBnd :: TyVarBndr flag -> TyVarBndrF flag Type
projectBnd = recTyVarBndr PlainTVF KindedTVF

{-| Rolls up one step of recursion into a `TyVarBndr`.

Embeds a `TyVarBndrF` onto the standard representation `TyVarBndr`.
-}
embedBnd :: TyVarBndrF flag Type -> TyVarBndr flag
embedBnd = recTyVarBndrF mkPlainTV mkKindedTV

---------------------------------------------------------------------------------------------------
-- Type Base Functor

{-|
Base functor for `Type`.

Use a hand-rolled base functor, as we then only need to handle the TH version inconsistencies
in the `embed` and `project` functions.
-}
type TypeF :: * -> *
data TypeF k = 
    ForallTF_ [TyVarBndrF Specificity k] [k] k
  | ForallVisTF_ [TyVarBndrF () k] k
  | AppTF_ k k
  | AppKindTF_ k k
  | SigTF_ k k
  | InfixTF_ k Name k
  | UInfixTF_ k Name k
  | PromotedInfixTF_ k Name k
  | PromotedUInfixTF_ k Name k
  | ImplicitParamTF_ String k
  | AtomicF_ Type
  | ParensTF_ k
  deriving stock (Functor, Foldable, Traversable, Generic)

{-# COMPLETE ForallTF,ForallVisTF, AppTF, AppKindTF, SigTF, InfixTF, UInfixTF, 
  PromotedInfixTF, PromotedUInfixTF, ImplicitParamTF, AtomicF, ParensTF #-}

pattern ForallTF :: [TyVarBndrF Specificity k] -> [k] -> k -> TypeF k
pattern ForallTF bnds ctx t <- ForallTF_ bnds ctx t
pattern ForallVisTF :: [TyVarBndrF () k] -> k -> TypeF k
pattern ForallVisTF bnds t <- ForallVisTF_ bnds t 
pattern AppTF :: k -> k -> TypeF k
pattern AppTF a b <- AppTF_ a b
pattern AppKindTF :: k -> k -> TypeF k
pattern AppKindTF a k <- AppKindTF_ a k
pattern SigTF :: k -> k -> TypeF k
pattern SigTF a k <- SigTF_ a k
pattern InfixTF :: k -> Name -> k -> TypeF k
pattern InfixTF  a n b      <- InfixTF_ a n b
pattern UInfixTF :: k -> Name -> k -> TypeF k
pattern UInfixTF a n b      <- UInfixTF_ a n b
pattern PromotedInfixTF :: k -> Name -> k -> TypeF k
pattern PromotedInfixTF a n b <- PromotedInfixTF_ a n b 
pattern PromotedUInfixTF :: k -> Name -> k -> TypeF k
pattern PromotedUInfixTF a n b <- PromotedUInfixTF_ a n b 
pattern ImplicitParamTF :: String -> k -> TypeF k
pattern ImplicitParamTF x a <- ImplicitParamTF_ x a
pattern AtomicF :: Type -> TypeF k
pattern AtomicF  a <- AtomicF_ a
pattern ParensTF :: k -> TypeF k
pattern ParensTF a <- ParensTF_ a

pattern VarTF :: Name -> TypeF k
pattern VarTF nm = AtomicF_ (VarT nm)

-------------------------------------------------------------------------------
-- Recursion Schemes Stuff
{-
To avoid adding a dependency on `recursion-schemes`, we re-implement some of
the core features from this library.
The originals can be found at:
https://hackage.haskell.org/package/recursion-schemes
-}

type Base :: * -> * -> *
type family Base t :: * -> *

type Recursive :: * -> Constraint
class Functor (Base t) => Recursive t where
  project :: t -> Base t t
  cata :: (Base t a -> a) -> t -> a
  cata f = c 
    where 
    c = f . fmap c . project

type Corecursive :: * -> Constraint
class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t

zygo :: Recursive t => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a
zygo f g = snd . cata (bisequence (f . fmap fst, g))

-------------------------------------------------------------------------------
-- Base Functor Recursive/Corecursive instances

-- | Newtype for `TH.Type`, so we can make an instance of Recursive and Corecursive.
type THType :: *
newtype THType = THType {getTHType :: TH.Type}

type instance Base THType = TypeF

projectType :: Type -> TypeF Type
projectType = fmap getTHType . project . THType

embedType :: TypeF Type -> Type
embedType = getTHType . embed . fmap THType

cataType :: (TypeF a -> a) -> Type -> a
cataType alg = cata alg . THType

zygoType :: (TypeF b -> b) -> (TypeF (b, a) -> a) -> Type -> a
zygoType α β = zygo α β . THType


instance Recursive THType where
  project :: THType -> Base THType THType
  project = fmap THType . go . getTHType
    where
    go :: Type -> Base THType Type
    go t = case t of
      ForallT bnds ctx a -> 
        ForallTF_ (map projectBnd bnds) ctx a
      AppT a b -> AppTF_ a b
      SigT a k -> SigTF_ a k
#if MIN_VERSION_template_haskell(2,11,0)
      InfixT a n b -> InfixTF_ a n b
      UInfixT a n b -> UInfixTF_ a n b
      ParensT k -> ParensTF_ k
#endif
#if MIN_VERSION_template_haskell(2,15,0)
      AppKindT a k -> AppKindTF_ a k
      ImplicitParamT x a -> ImplicitParamTF_ x a
#endif
#if MIN_VERSION_template_haskell(2,16,0)
      ForallVisT bnds a ->
        ForallVisTF_ (map projectBnd bnds) a
#endif
#if MIN_VERSION_template_haskell(2,19,0)
      PromotedInfixT a n b -> PromotedInfixTF_ a n b
      PromotedUInfixT a n b -> PromotedUInfixTF_ a n b
#endif
      a -> AtomicF_ a
    
instance Corecursive THType where
  embed :: Base THType THType -> THType
  embed = THType . go . fmap getTHType 
    where
    go :: TypeF Type -> Type
    go t = case t of
      ForallTF bnds ctx a -> 
        mkForallT (map embedBnd bnds) ctx a
      ForallVisTF bnds a ->
        mkForallVisT (map embedBnd bnds) a
      AppTF a b -> mkAppT a b
      AppKindTF a k -> mkAppKindT a k
      SigTF a k -> mkSigT a k
      InfixTF a n b -> mkInfixT a n b
      UInfixTF a n b -> mkUInfixT a n b
      PromotedInfixTF a n b -> mkPromotedInfixT a n b
      PromotedUInfixTF a n b -> mkPromotedUInfixT a n b
      ImplicitParamTF x a -> mkImplicitParamT x a
      ParensTF k -> mkParensT k
      AtomicF a -> a


---------------------------------------------------------------------------------------------------
-- Smart Types and Constructors for version agnosticism

{-| A type variable binder.

__Do not__ pattern match on this, instead use `recTyVarBndr`, which safely handles pattern matching in different
versions of template haskell.

/Note:/ In TemplateHaskell < 2.17, the flag parameter is ignored.
-}
#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr :: * -> *
type TyVarBndr flag = THAll.TyVarBndr flag
#else
type TyVarBndr flag = THAll.TyVarBndr 
#endif

{-| The specificity of a binding; whether it is inferred or given by a user.

__Note:__ In TemplateHaskell < 2.17, this is unit.
-}
type Specificity :: *
#if MIN_VERSION_template_haskell(2,17,0)
type Specificity = THAll.Specificity
#else
type Specificity = ()
#endif

{-| Version-safe way to pattern match on `TyVarBndr`.

First case is for `TH.PlainTV`, the second for `TH.KindedTV`.
-}
recTyVarBndr 
  :: (Name -> flag -> a) -- The `PlainTV` case
  -> (Name -> flag -> Kind -> a) -- The `KindedTV` case
  -> TyVarBndr flag 
  -> a
#if MIN_VERSION_template_haskell(2,17,0)
recTyVarBndr f _ (THAll.PlainTV nm flag) = f nm flag
recTyVarBndr _ g (THAll.KindedTV nm flag k) = g nm flag k
#else
  -- TODO: this is quite naughty
recTyVarBndr f _ (THAll.PlainTV nm) = f nm undefined
recTyVarBndr _ g (THAll.KindedTV nm k) = g nm undefined k
#endif

{-| Version-safe `TH.PlainTV` constructor for `TyVarBndr`.

In Template Haskell < 2.17, @flag@ can be assumed to be @()@.
-}
mkPlainTV :: Name -> flag -> TyVarBndr flag

{-| Version-safe `TH.KindedTV` constructor for `TyVarBndr`.

In Template Haskell < 2.17, @flag@ can be assumed to be @()@.
-}
mkKindedTV :: Name -> flag -> Type -> TyVarBndr flag
#if MIN_VERSION_template_haskell(2,17,0)
mkPlainTV  = THAll.PlainTV
mkKindedTV = THAll.KindedTV
#else
mkPlainTV  n ~_ = THAll.PlainTV n
mkKindedTV n ~_ = THAll.KindedTV n
#endif

mkVarT :: Name -> Type
mkVarT = VarT

mkConT :: Name -> Type
mkConT = ConT

mkPromotedT :: Name -> Type
mkPromotedT = PromotedT

-- | Smart constructor for `ForallT`. Version-safe.
mkForallT :: [TyVarBndr Specificity] -> Cxt -> Type -> Type
mkForallT = ForallT
-- | Smart constructor for `SigT`. Version-safe.
mkAppT :: Type -> Type -> Type
mkAppT = AppT
-- | Smart constructor for `SigT`. Version-safe.
mkSigT :: Type -> Kind -> Type
mkSigT = SigT

{-| Smart constructor for `InfixT`.

Equal to `undefined` for template haskell versions < 2.11, so ensure any code that
uses this will need only be run in versions ≥ 2.11 .
-}
mkInfixT :: Type -> Name -> Type -> Type
{-| Smart constructor for `UInfixT`.

Equal to `undefined` for template haskell versions < 2.11, so ensure any code that
uses this will need only be run in versions ≥ 2.11 .
-}
mkUInfixT :: Type -> Name -> Type -> Type

{-| Smart constructor for `ParensT`.

Equal to `undefined` for template haskell versions < 2.11, so ensure any code that
uses this will need only be run in versions ≥ 2.11 .
-}
mkParensT :: Type -> Type
#if MIN_VERSION_template_haskell(2,11,0)
mkParensT = ParensT
mkUInfixT = UInfixT
mkInfixT = InfixT
#else
mkUInfixT = undefined
mkParensT = undefined
mkInfixT = undefined
#endif

{-| Smart constructor for `AppKindT`.

Equal to `undefined` for template haskell versions < 2.15, so ensure any code that
uses this will need only be run in versions ≥ 2.15 .
-}
mkAppKindT :: Type -> Type -> Type

{-| Smart constructor for `ImplicitParamT`.

Equal to `undefined` for template haskell versions < 2.15, so ensure any code that
uses this will need only be run in versions ≥ 2.15 .
-}
mkImplicitParamT :: String -> Type -> Type
#if MIN_VERSION_template_haskell(2,15,0)
mkAppKindT = AppKindT
mkImplicitParamT = ImplicitParamT
#else
mkAppKindT = undefined
mkImplicitParamT = undefined
#endif

{-| Smart constructor for `ForallVisT`.

Equal to `undefined` for template haskell versions < 2.16, so ensure any code that
uses this will need only be run in versions ≥ 2.16 .
-}
mkForallVisT :: [TyVarBndr ()] -> Type -> Type
#if MIN_VERSION_template_haskell(2,16,0)
mkForallVisT bnds tp = ForallVisT bnds tp
#else
mkForallVisT bnds tp = undefined
#endif

{-| Smart constructor for `PromotedInfixT`.

Equal to `undefined` for template haskell versions < 2.19, so ensure any code that
uses this will need only be run in versions ≥ 2.19 .
-}
mkPromotedInfixT :: Type -> Name -> Type -> Type

{-| Smart constructor for `PromotedUInfixT`.

Equal to `undefined` for template haskell versions < 2.19, so ensure any code that
uses this will need only be run in versions ≥ 2.19 .
-}
mkPromotedUInfixT :: Type -> Name -> Type -> Type
#if MIN_VERSION_template_haskell(2,19,0)
mkPromotedInfixT = PromotedInfixT
mkPromotedUInfixT = PromotedUInfixT
#else
mkPromotedInfixT = undefined
mkPromotedUInfixT = undefined
#endif


#if !(MIN_VERSION_template_haskell(2,17,0))

{-
All of this is ported from:
https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/src/Language.Haskell.TH.Syntax.html

-}

class Monad m => Quote m where
  newName :: String -> m Name

instance Quote IO where
  newName s = do { n <- atomicModifyIORef' counter (\x -> (x + 1, x))
                 ; pure (mkNameU s n) }
instance Quote Q where
  newName = qNewName

instance (Semigroup a) => Semigroup (Q a) where
  (<>) = liftA2 (<>)
instance (Monoid a) => Monoid (Q a) where
  mempty = pure mempty


#endif


#if !(MIN_VERSION_template_haskell(2,17,0))

data DocLoc = DeclDoc Name

getDoc :: DocLoc -> Q (Maybe String)
getDoc _ = pure Nothing

putDoc :: DocLoc -> String -> Q ()
putDoc _ _ = pure ()

-- Is this awful?
pattern MulArrowT = ArrowT


#endif
