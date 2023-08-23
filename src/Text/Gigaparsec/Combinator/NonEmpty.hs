{-# LANGUAGE Safe #-}
module Text.Gigaparsec.Combinator.NonEmpty (some, someTill, sepBy1, sepEndBy1, endBy1) where

import Text.Gigaparsec (Parsec, liftA2, many, notFollowedBy)
import Text.Gigaparsec.Combinator qualified as Combinator (manyTill, sepEndBy1)

import Data.List.NonEmpty as NonEmpty (NonEmpty((:|)), fromList)

infixl 4 <:|>
(<:|>) :: Parsec a -> Parsec [a] -> Parsec (NonEmpty a)
(<:|>) = liftA2 (:|)

{-
infixl 4 <<|>
(<<|>) :: Parsec a -> Parsec (NonEmpty a) -> Parsec (NonEmpty a)
(<<|>) = liftA2 (<|)
-}

some :: Parsec a -> Parsec (NonEmpty a)
some p = p <:|> many p

someTill :: Parsec a -> Parsec end -> Parsec (NonEmpty a)
someTill p end = notFollowedBy end *> (p <:|> Combinator.manyTill p end)

sepBy1 :: Parsec a -> Parsec sep -> Parsec (NonEmpty a)
sepBy1 p sep = p <:|> many (sep *> p)

endBy1 :: Parsec a -> Parsec sep -> Parsec (NonEmpty a)
endBy1 p sep = some (p <* sep)

sepEndBy1 :: Parsec a -> Parsec sep -> Parsec (NonEmpty a)
sepEndBy1 p sep = NonEmpty.fromList <$> Combinator.sepEndBy1 p sep
