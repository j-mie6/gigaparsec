{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{- |
Module      : Text.Gigaparsec.Token.Patterns
Description : Template Haskell generators to help with patterns
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module is currently experimental, and may have bugs depending on the version
of Haskell, or the extensions enabled. Please report any issues to the maintainers.

@since 0.2.2.0
-}
module Text.Gigaparsec.Internal.Token.Patterns.LexerCombinators (
  module Text.Gigaparsec.Internal.Token.Patterns.LexerCombinators,
) where

import safe Text.Gigaparsec.Internal.Token.Lexer (
  Lexeme (
    charLiteral,
    multiStringLiteral,
    names,
    rawMultiStringLiteral,
    rawStringLiteral,
    stringLiteral,
    symbol
  ),
  Lexer (lexeme, space),
  Space,
 )
import safe Text.Gigaparsec.Internal.Token.Names (Names)
import safe Text.Gigaparsec.Internal.Token.Symbol (Symbol)
import safe Text.Gigaparsec.Internal.Token.Text (TextParsers)

import Text.Gigaparsec.Internal.TH.DecUtils (funDsingleClause)
import Text.Gigaparsec.Internal.TH.TypeUtils (removeUnusedTVars, sanitiseBndrStars, sanitiseTypeStars)

import Text.Gigaparsec.Internal.TH.VersionAgnostic (
  Dec, DocLoc(DeclDoc), Exp, Inline (Inline), Phases (AllPhases), Q, Quasi (qRecover), 
  Quote (newName), Type (ForallT), addModFinalizer, getDoc, isInstance, 
  nameBase, putDoc, reifyType,
  RuleMatch (FunLike),
  Type (AppT, ArrowT, ForallVisT), 
  pattern MulArrowT,
  clause,
  funD,
  normalB,
  pprint,
  pragInlD,
  sigD,
  varE,
  )

import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Text.Gigaparsec.Internal.TH.VersionAgnostic (Name)
import Text.Gigaparsec.Token.Lexer qualified as Lexer

{-|
Generates the specified lexer combinators using the quoted lexer.

==== __Usage:__

> import Text.Gigaparsec.Token.Lexer qualified as Lexer
> import Text.Gigaparsec.Token.Lexer (Lexer)
> lexer :: Lexer
> $(lexerCombinators [| lexer |] ['Lexer.lexeme, 'Lexer.fully, 'Lexer.identifier, 'Lexer.stringLiteral])

This will generate the following combinators/functions:

> lexeme :: Lexeme
> fully :: ∀ a . Parsec a -> Parsec a
> identifier :: Parsec String
> stringLiteral :: TextParsers String

These will behave like their counterparts in "Text.Gigaparsec.Token.Lexer", except they will not need
a 'Lexer' (or its subcomponents) as an argument.

@since 0.4.0.0
-}
lexerCombinators
  :: Q Exp -- The lexer
  -> [Name] -- The combinators to generate
  -> Q [Dec]
lexerCombinators lexer ns = lexerCombinatorsWithNames lexer (zip ns (map nameBase ns))

{-|
Generates the specified lexer combinators with the specified names using the quoted lexer.

==== __Usage:__

> import Text.Gigaparsec.Token.Lexer qualified as Lexer
> import Text.Gigaparsec.Token.Lexer (Lexer)
> lexer :: Lexer
> $(lexerCombinatorsWithNames [| lexer |] [('Lexer.lexeme, "myLexeme"), ('Lexer.fully, "myFully")])

This will generate the following combinators/functions:

> myLexeme :: Lexeme
> myFully :: ∀ a . Parsec a -> Parsec a

These will behave like their counterparts in "Text.Gigaparsec.Token.Lexer", except they will not need
a 'Lexer' (or its subcomponents) as an argument.

@since 0.4.0.0
-}
lexerCombinatorsWithNames ::
  Q Exp -> -- The lexer
  [(Name, String)] -> -- The combinators to generate
  Q [Dec]
lexerCombinatorsWithNames lexer = fmap concat . traverse (uncurry (lexerCombinatorWithName lexer))

{-|
Create a single lexer combinator with a given name, defined in terms of the lexer.
-}
lexerCombinatorWithName
  :: Q Exp
  -> Name -- The name of the old combinator
  -> String -- the new name of the combinator
  -> Q [Dec]
lexerCombinatorWithName lexer old nm = do
  newTp <- getLexerCombinatorType old True
  mkLexerCombinatorDec lexer nm old newTp

{-| 
Constructs the combinator using the given type.
Calculates the definition of the combinator using a typeclass (if possible).
-}
mkLexerCombinatorDec ::
  -- | The quoted Lexer
  Q Exp ->
  -- | The name of the combinator to generate
  String ->
  -- | The quoted name of the original combinator
  Name ->
  -- | The return type of the new combinator
  Type ->
  Q [Dec]
mkLexerCombinatorDec lexer nm old tp = do
  newX <- newName nm
  oldDocs <- getDoc (DeclDoc old)
  let newDocs = fromMaybe "" oldDocs
  addModFinalizer $ putDoc (DeclDoc newX) newDocs
  sequence
    [ pragInlD newX Inline FunLike AllPhases
    , sigD newX (pure tp)
    , funD newX [clause [] (normalB [|project $(varE old) $lexer|]) []]
    ]

{-| 
Constructs the combinator using the given type.
Calculates the definition of the combinator using a typeclass (if possible).
-}
mkLexerCombinatorDecWithProj ::
  -- | The quoted Lexer
  Q Exp ->
  -- | The name of the combinator to generate
  String ->
  -- | @old@, The quoted name of the original combinator
  Name ->
  -- | The return type of the new combinator
  Q Type ->
  -- | projection to precompose the @old@ combinator with
  Q Exp ->
  Q (Name, [Dec]) -- The name of the new combinator and its declaration
mkLexerCombinatorDecWithProj lexer nm old tp proj = do
  newX <- newName nm
  oldDocs <- getDoc (DeclDoc old)
  let newDocs = fromMaybe "" oldDocs
  addModFinalizer $ putDoc (DeclDoc newX) newDocs
  (newX,)
    <$> sequence
      [ pragInlD newX Inline FunLike AllPhases
      , sigD newX tp
      , funDsingleClause newX [|project ($(varE old) . $proj) $lexer|]
      ]

{-| 
Figures out the type of the combinator minus the domain; this will be one of a 'Lexer' component, or any other subcomponents (e.g. 'Symbol' or 'Space').
Calculates the domain type, and the return type of the new combinator.
The boolean flag set to True means one should ensure the domain type gives a specific combinator,
and doesn't lead to an ambiguous return type.
-}
getLexerCombinatorType :: Name -> Bool -> Q Type
getLexerCombinatorType old checkType = do
  tp <- reifyType old
  (prefix, dom, _, cod) <-
    fail (notFunctionTypeMsg old tp) `qRecover` fnTpDomain tp
  let newTp = prefix cod
  b <- isInstance ''LexerField [dom]
  if checkType && not b
    then catchErrors dom newTp
    else return $ prefix cod
 where
  -- If the quoted name is not at least a function type, then there's no real way to define the combinator :p
  notFunctionTypeMsg :: Name -> Type -> String
  notFunctionTypeMsg x tp = concat ["Constant `", nameBase x, "` does not have a function type: ", show tp]

  -- Preventative Errors: catch the cases someone tries to define one of the String or Integer parsers
  -- they should do these manually or with the bespoke generators!
  catchErrors :: Type -> Type -> Q a
  catchErrors dom newTp
    | old == 'Lexer.ascii = failStringParser old
    | old == 'Lexer.unicode = failStringParser old
    | old == 'Lexer.latin1 = failStringParser old
    | otherwise = fail (notLexerFieldMsg old dom)

  -- Message to give to the user when they give a TextParser field, as there is no way to disambiguate
  -- exactly *which* TextParser they want.
  -- And there is no point in implementing a way for the user to ask for this;
  -- at that point, it would be no more work on the user's end than were they to write a manual definition.
  failStringParser :: Name -> Q a
  failStringParser nm =
    fail $
      concat
        [ "Cannot derive a lexer combinator for `"
        , nameBase nm
        , "`, as there are many possible "
        , pprint ''TextParsers
        , " to define it in terms of, including:"
        , pprint 'stringLiteral
        , ", "
        , pprint 'rawStringLiteral
        , ", "
        , pprint 'multiStringLiteral
        , ", and "
        , pprint 'rawMultiStringLiteral
        , "."
        , "\n You will need to manually define this combinator, as you are then able to pick which TextParser it should use."
        ]

  -- If the quoted name is not a recognised lexer field, then we should tell the user as much.
  -- The error may be due to being able to disambiguate the field, rather than the field not existing.
  notLexerFieldMsg :: Name -> Type -> String
  notLexerFieldMsg x tp =
    concat
      [ "Cannot produce a lexer combinator for function: "
      , nameBase x
      , "."
      , "\n This is because the type: `"
      , pprint tp
      , "` cannot be used to give a precise combinator, either because it does not refer to "
      , "any fields of a `Lexer`, or because it ambiguously refers to many fields of a `Lexer`."
      , "\n Some fields of the `Lexer` share the same type, so there are multiple possible candidate combinators for a particular field."
      , " For example: "
      , "\n   - `decimal`, `hexadecimal`,... all have type `IntegerParsers canHold -> Parsec Integer`."
      , "\n   - `ascii`, `unicode`, ... all have type `TextParsers t -> Parsec t`."
      ]

---------------------------------------------------------------------------------------------------
-- Util functions

{-|
Denote the type of an arrow; it is either normal or linear.
-}
type ArrowTp :: *
data ArrowTp = StdArrow | LinearArrow

{-|
Get the domain of a function type.
Keep any prefixed constraints and type variable quantifiers as a prefixing function.
-}
fnTpDomain
  :: Type
  -> Q (Type -> Type, Type, ArrowTp, Type)
-- The head of the type, includes any preceding constraints
-- and foralls. this is a function which prefixes the given type with the constraints/foralls
-- The domain and codomain of the type
fnTpDomain x = do
  (a, (b, c, d)) <- fnTpDomain' =<< sanitiseTypeStars x
  return (removeUnusedTVars . a, b, c, d)
 where
  fnTpDomain' (ForallT bnds ctx tp) = do
    bnds' <- sanitiseBndrStars bnds
    first (ForallT bnds' ctx .) <$> fnTpDomain' tp
  fnTpDomain' (ForallVisT bnds tp) = do
    bnds' <- sanitiseBndrStars bnds
    first (ForallVisT bnds' .) <$> fnTpDomain' tp
  fnTpDomain' (AppT (AppT ArrowT a) b) =
    return (id, (a, StdArrow, b))
  fnTpDomain' (AppT (AppT MulArrowT a) b) =
    return (id, (a, LinearArrow, b))
  fnTpDomain' tp =
    fail
      ("Type of given function is not a function type: " ++ show tp)

---------------------------------------------------------------------------------------------------
-- Lexer Field

{- |
@a@ is a `LexerField` when it is the type of a component or subcomponent of the `Lexer` type.
This includes things like `Lexeme` and `Symbol`.

Avoid writing instances for:
- IntegerParsers
- TextParsers String

As this leads to ambiguous projections if users try to generate combinators for, e.g., 'Lexer.decimal.
There are two possible instances here, one for `integer`, the other for `natural`, and there is no way to disambiguate here.
By avoiding writing these instances, we can give the user a more informative error message should they try this.
-}
type LexerField :: * -> Constraint
class LexerField a where
  project :: (a -> b) -> (Lexer -> b)

type LexerProj :: * -> * -> *
type LexerProj a b = (a -> b) -> (Lexer -> b)

instance LexerField Lexer where
  {-# INLINE project #-}
  project :: (Lexer -> b) -> (Lexer -> b)
  project = id

---------------------------------------------------------------------------------------------------
-- Lexemes

instance LexerField Lexeme where
  {-# INLINE project #-}
  project :: (Lexeme -> b) -> (Lexer -> b)
  project f = f . lexeme

instance LexerField Symbol where
  {-# INLINE project #-}
  project :: (Symbol -> b) -> (Lexer -> b)
  project f = f . symbol . lexeme

instance LexerField Names where
  {-# INLINE project #-}
  project :: (Names -> b) -> (Lexer -> b)
  project f = f . names . lexeme

instance LexerField (TextParsers Char) where
  {-# INLINE project #-}
  project :: (TextParsers Char -> b) -> (Lexer -> b)
  project f = f . charLiteral . lexeme

---------------------------------------------------------------------------------------------------
-- Space

instance LexerField Space where
  {-# INLINE project #-}
  project :: (Space -> b) -> (Lexer -> b)
  project f = f . space
