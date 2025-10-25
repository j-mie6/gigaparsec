{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}
module Shared.FlatParse.Utils where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.ByteString qualified as B hiding (unpack)
import Data.ByteString.Char8 qualified as B

import FlatParse.Basic hiding (Parser)
import FlatParse.Basic qualified as FP
import FlatParse.Common.Parser (PureMode)
import Language.Haskell.TH
import Language.Haskell.TH (Body(NormalB))
import Data.Foldable (foldl')
import Control.Applicative ((<**>), liftA)
import Control.Monad ((>=>), (<=<))
import Control.Applicative qualified as App

type Parser = ParserT PureMode ()

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

chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op =
  let (go :: Parser a) = p <**> ((flip <$> op <*> go) <|> pure id)
  in  go
  -- let go k = flip <$> op
  --   -- p <**> (flip <$> op)
  -- in _
  where
    go2 :: Parser a
    go2 = do
      let x = p <**> ((op <*> go2) <|> pure id)
      _
    go5 :: Parser ((a -> a) -> a)
    go5 =
      let pop expr = ((op <*> expr) <**> go5) <|> expr
      in  pure _

    go4 :: Parser ((a -> a) -> a)
    go4 =
      let pop = (p <**> op)
      in  pop <**> go4

    go3 :: (a -> a) -> Parser a
    go3 k = do
      expr <- p
      (do f <- op
          go3 (f expr))
        <|> pure (k expr)

    go :: a -> (a -> a) -> Parser a
    go atomL k = do
      f <- op
      atomR <- p
      go atomR (f (k atomL))
  -- chainPost p (flip <$> op <*> p)

chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op =
  let (go :: Parser a) = p <**> ((flip <$> op <*> go) <|> pure id)
  in  go

chainPre :: Parser (a -> a) -> Parser a -> Parser a
chainPre op p = flip (foldr ($)) <$> many op <*> p

chainPost :: Parser a -> Parser (a -> a) -> Parser a
chainPost p op = foldl' (flip ($)) <$> p <*> many op

(<~>) :: Parser a -> Parser b -> Parser (a, b)
(<~>) = liftA2 (,)

