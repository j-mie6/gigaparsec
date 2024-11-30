{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Internal.TH.DecUtils (
  module Text.Gigaparsec.Internal.TH.DecUtils
  ) where
import Text.Gigaparsec.Internal.TH.VersionAgnostic (Quote, Name, Exp, Dec)
import Language.Haskell.TH (normalB, clause, funD)
    
funDsingleClause :: Quote m => Name -> m Exp -> m Dec
funDsingleClause x body = funD x [clause [] (normalB body) []]
