{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK hide #-}


module Text.Gigaparsec.Internal.TH.DecUtils (
  module Text.Gigaparsec.Internal.TH.DecUtils
  ) where
import Text.Gigaparsec.Internal.TH.VersionAgnostic (Q, Name, Exp, Dec)
import Language.Haskell.TH (normalB, clause, funD)
    
funDsingleClause :: Name -> Q Exp -> Q Dec
funDsingleClause x body = funD x [clause [] (normalB body) []]
