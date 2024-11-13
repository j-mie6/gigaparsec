{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Gigaparsec.Internal.Token.InputStream where
import Data.Kind (Constraint)


-- type InputStream :: * -> *
-- data InputStream s = InputStream {
--     isEmpty :: s -> Bool
--   }
type InputStream :: * -> Constraint
class InputStream s where
  isEmptyInputStream :: s -> Bool

instance InputStream [a] where
  isEmptyInputStream :: [a] -> Bool
  isEmptyInputStream [] = True
  isEmptyInputStream _  = False

