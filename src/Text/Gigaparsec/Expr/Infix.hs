{-# LANGUAGE Safe #-}
{-|
Module      : Text.Gigaparsec.Expr.Infix
Description : This module contains the very useful chaining family of combinators, 
              which are mostly used to parse operators and expressions of varying fixities.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module contains the very useful chaining family of combinators, 
              which are mostly used to parse operators and expressions of varying fixities.

It is a lower-level API than 'Text.Gigaparsec.Expr.precedence'.

Compared with the combinators in 'Text.Gigaparsec.Expr.Chain', 
these allow for more freedom in the type of the values and the operators.
-}
module Text.Gigaparsec.Expr.Infix (
  -- ** Binary Operator Chains
  {-|
  These combinators allow for the chaining together of values and binary operators in either left-, 
  right- or non-associative application.
  -}
  infixl1,
  infixr1,
  infixn1,
  -- ** Unary Operator Chains
  {-|
  These combinators allow for the chaining together, and application, of multiple 
  prefix or postfix unary operators to a single value.
  -}
  prefix,
  postfix,
  -- ** Module Re-export
  {-|
  This should be removed.
  -}
  module Text.Gigaparsec.Expr.Infix) where

import Text.Gigaparsec (Parsec, (<|>), (<**>))

{-|
This combinator handles left-associative parsing, and the application of, 
__zero__ or more binary operators between __one__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ₋₁@, with left-associative application: 
fₙ₋₁ (fₙ₋₂ (..(f₁ x₁ x₂)..) xₙ₋₁) xₙ.
This application is then returned as the result of the combinator. 
If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.

Compared with 'Text.Gigaparsec.Expr.Chain.chainl1', 
this combinator allows the types of the operators to more accurately encode their associativity in their types. 
However, 'Text.Gigaparsec.Expr.Chain.chainl1', in which @a@ and @b@ must match, 
allows for more flexibility to change the associativity.
-}
infixl1 :: (a -> b)             -- ^ a function converting the value type @a@ into the result type @b@.
        -> Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (b -> a -> b) -- ^ @op@, the operator between each value.
        -> Parsec b             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@, 
                                -- and applies their results left-associatively.
infixl1 wrap p op = postfix wrap p (flip <$> op <*> p)

{-|
This combinator handles right-associative parsing, and the application of, 
__zero__ or more binary operators between __one__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ@, with right-associative application: 
@f₁ x₁ (f₂ x₂ (...(fₙ₋₁ xₙ₋₁ xₙ)...))@.
This application is then returned as the result of the combinator. 
If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.

Compared with 'Text.Gigaparsec.Expr.Chain.chainr1', 
this combinator allows the types of the operators to more accurately encode their associativity in their types. 
However, 'Text.Gigaparsec.Expr.Chain.chainr1', in which @a@ and @b@ must match, 
allows for more flexibility to change the associativity.
-}
infixr1 :: (a -> b)             -- ^ a function converting the value type @a@ into the result type @b@.
        -> Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (a -> b -> b) -- ^ @op@, the operator between each value.
        -> Parsec b             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@,
                                --  and applies their results right-associatively.
infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

{-|
This combinator handles non-associative parsing, and the application of, 
__zero__ or __one__ binary operators between __one__ or __two__ values.

First parse @p@.
Then:

* If this not is followed by an @op@, simply return @p@.
* Otherwise, parse this @op@ followed by a single @p@. 
  Then ensure that this is not followed by a further @op@, to enforce non-associativity.
  The results of the @p@s, @x@ and @y@, are combined with the result of @op@, 
  @f@ with the application @f x y@.
  This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
This combinator also fails if the second @p@ is followed by another @op@.
-}
infixn1 :: (a -> b)             -- ^ a function converting the value type @a@ into the result type @b@.
        -> Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (a -> a -> b) -- ^ @op@, the operator between each value.
        -> Parsec b             -- ^ a parser that parses @p@, @op@ and then @p@, and applies the 
                                -- result of @op@ to those of @p@ in the same order.
infixn1 wrap p op = p <**> (flip <$> op <*> p <|> pure wrap)

{-|
This combinator handles right-assocative parsing, and application of, 
__zero__ or more prefix unary operators to a single value.

First parse many repeated @op@s. 
When there are no more @op@s left to parse, parse a single @p@. 
The result of @p@, @x@, is applied first to @wrap@, and then to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @fₙ@ is applied first and @f₁@ last: @f₁ (f₂ (..(fₙ (wrap x))..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
prefix  :: (a -> b)        -- ^ @wrap@ a function converting the value type @a@ into the result type @b@ 
        -> Parsec (b -> b) -- ^ @op@, the prefix operator to repeatedly parse before @p@.
        -> Parsec a        -- ^ @p@, the single value to be parsed
        -> Parsec b        -- ^ a parser that parses many @op@s, and a final @p@, and applies all 
                           -- of the results right-associatively.
prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

{-|
This combinator handles left-assocative parsing, and application of, 
__zero__ or more postfix unary operators to a single value.

First parse a single @p@.
Then, parse many repeated @op@s.
The result of @p@, @x@, is applied first to @wrap@, and then to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @f₁@ is applied first and @fₙ@ last: @fₙ( fₙ₋₁(..f₁ (wrap x)..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
postfix :: (a -> b)        -- ^ a function converting the value type @a@ into the result type @b@ 
        -> Parsec a        -- ^ @p@, the single value to be parsed
        -> Parsec (b -> b) -- ^ @op@, the postfix operator to repeatedly parse after @p@.
        -> Parsec b        -- ^ a parser that parses many @op@s, and a final @p@, and applies all 
                           -- of the results left-associatively.
postfix wrap p op = wrap <$> p <**> rest
  where rest = flip (.) <$> op <*> rest <|> pure id
