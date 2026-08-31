{-# LANGUAGE TemplateHaskell, MagicHash #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE RoleAnnotations #-}
module Shared.FlatParse.Extended where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.ByteString qualified as B hiding (unpack)
import Data.ByteString.Char8 qualified as B

import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import FlatParse.Basic hiding (Parser)
import FlatParse.Basic qualified as FP
import FlatParse.Common.Parser (PureMode)
import Language.Haskell.TH
import Language.Haskell.TH (Body(NormalB))
import Data.Foldable (foldl')
import Control.Applicative ((<**>), liftA)
import Control.Monad ((>=>), (<=<))
import Control.Applicative qualified as App
import Data.Kind (Constraint)

type Parser = ParserT PureMode ()

runParserString :: ParserT PureMode e a -> String -> FP.Result e a
runParserString p xs = 
  let bs = T.encodeUtf8 (T.pack xs)
  in  FP.runParser p bs

switchFromSet :: Set String -> (String -> Code Q (Parser a)) -> Code Q (Parser a)
switchFromSet xs p = unsafeCodeCoerce (FP.switch caseStat)
  where
    caseStat :: Q Exp
    caseStat = CaseE (UnboundVarE (mkName "_")) <$> (mapM matchP (Set.toList xs))

    matchP x = Match (patt x) <$> (patbody x) <*> pure []
    patt x = (LitP (StringL x))
    patbody x = NormalB <$> unTypeCode (p x)

pfoldl1 :: (b -> a -> b) -> b -> Parser a -> Parser b
pfoldl1 f k p = foldl' f k <$> some p


sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  withOption p (\x -> (x:) <$> many (sep *> p)) (pure [])

{-|
Chain with associativity given by operator parser which 
also gives the precedence of the operator parsed.

@a@ is the result, @p@ is the precedence.
Same precedence is left associative.
-}
chainl1Prec :: forall a p. 
      (Ord p, Bounded p) 
  =>  Parser a 
  ->  Parser (p, a -> a -> a) 
  ->  Parser a
chainl1Prec p op = go maxBound id
  where
  go :: p -> (a -> a) -> Parser a
  go !prevPrec !k = do
    !x <- fmap k p
    flip (withOption op) (pure x) \ !(!curPrec, !f) -> 
      go curPrec $!
        if prevPrec >= curPrec 
          then f (k x) 
          else (k . f x)

chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op =
  go id
  where
  go :: (a -> a) -> Parser a
  go !k = do
    !x <- fmap k p
    withOption op (\ !f -> go (f x) ) (pure x)


chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op =
  let (go :: Parser a) = p <**> ((flip <$> op <*> go) <|> pure id)
  in  go

prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
prefix wrap op p = chainPre op (wrap <$> p)

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = chainPost (wrap <$> p) op

chainPre :: Parser (a -> a) -> Parser a -> Parser a
chainPre op p = manyComp (ManyComp (.)) op <*> p
  -- let go !k = withOption op (\ !f -> go (k . f)) (pure k)
  -- in  go id <*> p
  -- withOption op (<$> chainPre op p) p

chainPost :: Parser a -> Parser (a -> a) -> Parser a
chainPost p op = p <**> manyComp (ManyComp (flip (.))) op
  -- let go !k = withOption op (\ !f -> go (f . k)) (pure k)
  -- in  p <**> go id

type role ManyComp representational
type ManyComp :: * -> *
newtype ManyComp a = ManyComp {
  {-|
    @runManyComp k f@ will take the previously generated continuation @k@, and choose how to compose it with @f@.
    
    For example, it may return the continuation @k . f@ to /precompose/, or @f . k@ to /postcompose/.
  -}
  runManyComp 
    :: (a -> a) -- The continuation made thus far
    -> (a -> a) -- The new function to compose with 
    -> (a -> a)
  }


manyComp 
  :: ManyComp a
  -> Parser (a -> a)
  -> Parser (a -> a)
manyComp !comp op = 
  let go !k = withOption op (\ !f -> go (runManyComp comp k f)) (pure k)
  in  go id

(<~>) :: Parser a -> Parser b -> Parser (a, b)
(<~>) = liftA2 (,)

(<+>) :: Parser a -> Parser b -> Parser (Either a b)
p <+> q = (Left <$> p) <|> (Right <$> q)


-- {-|
-- CPS version of `Selective` branch; does not backtrack.

-- @branchC p q f g@ will first run @p@:

--   - if @p@ succeeds, run @f@ and apply the resulting function to the result of @p@
--   - if @p@ fails, run @q@, then @g@, and apply the result of @g@ to that of @q@.
-- -}
-- branchC :: Parser a -> Parser b -> Parser (a -> c) -> Parser (b -> c) -> Parser c
-- branchC p q pf qf = FP.ParserT \fp eob s st -> case FP.runParserT# p fp eob s st of
--   FP.OK# st' x s -> let !f = runParserT# pf fp eob s st' in (_? f x)
--   FP.Fail# st' -> _
--   FP.Err# st' e -> FP.Err# st' e



switchTyped :: Code Q (String -> Parser a) -> Code Q (Parser a)
switchTyped = switchTypedMPost Nothing

switchTypedPost :: Code Q (Parser ()) -> Code Q (String -> Parser a) -> Code Q (Parser a)
switchTypedPost p = switchTypedMPost (Just p)

switchTypedMPost :: Maybe (Code Q (Parser ())) -> Code Q (String -> Parser a) -> Code Q (Parser a)
switchTypedMPost post code = 
  bindCode (unTypeCode code) $ \e -> 
    case e of
      LamCaseE cases -> 
        let exp = CaseE (UnboundVarE (mkName "_")) cases
        in  unsafeCodeCoerce (switchWithPost (unTypeCode <$> post) (pure exp))
      _ -> liftCode (fail "FlatParse.Utils.switchTyped: expected a `\\case` expression.")

