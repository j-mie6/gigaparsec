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
