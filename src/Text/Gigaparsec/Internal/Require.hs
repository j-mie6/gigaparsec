{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
module Text.Gigaparsec.Internal.Require (require, RequirementUnsatisfied) where

import Control.Exception (Exception, throw)

type RequirementUnsatisfied :: *
data RequirementUnsatisfied = RequirementUnsatisfied { func :: !String
                                                     , msg :: !String
                                                     }

instance Show RequirementUnsatisfied where
  show :: RequirementUnsatisfied -> String
  show RequirementUnsatisfied{..} = "requirement unsatisfied, " ++ msg ++ " (" ++ func ++ ")"

instance Exception RequirementUnsatisfied

require :: Bool -> String -> String -> a -> a
require True _ _ = id
require False func msg = throw (RequirementUnsatisfied func msg)
