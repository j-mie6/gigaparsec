{-# LANGUAGE TemplateHaskell #-}
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
import Control.Applicative ((<**>))

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

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = chainPost p (flip <$> op <*> p)

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = 
  let go = p <**> ((flip <$> op <*> go) <|> pure id) 
  in  go

chainPre :: Parser (a -> a) -> Parser a -> Parser a
chainPre op p = flip (foldr ($)) <$> many op <*> p

chainPost :: Parser a -> Parser (a -> a) -> Parser a
chainPost p op = foldl' (flip ($)) <$> p <*> many op

(<~>) :: Parser a -> Parser b -> Parser (a, b)
(<~>) = liftA2 (,)

