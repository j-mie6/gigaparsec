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

See "Text.Gigaparsec.Expr.Infix" for combinators that allow for more 
freedom in the type of the values and the operators.
-}
module Text.Gigaparsec.Expr.Chain (
  -- ** Binary Operator Chains
  {-|
  These combinators allow for the chaining together of values and binary operators in either left-, 
  right- or non-associative application.
  -}
  -- *** Left-Associative Binary Operators
  chainl,
  chainl1,
  -- *** Right-Associative Binary Operators
  chainr1,
  chainr,
  -- *** Non-Associative Binary Operators
  chainn1,
  -- ** Unary Operator Chains
  {-|
  These combinators allow for the chaining together, and application, of multiple 
  prefix or postfix unary operators to a single value.
  -}
  -- *** Prefix Operators
  prefix,
  prefix1,
  -- *** Postfix Operators
  postfix,
  postfix1,
  -- ** Module Re-export
  -- | This should be removed.
  module Text.Gigaparsec.Expr.Chain) where

import Text.Gigaparsec (Parsec, (<|>), (<**>))
import Text.Gigaparsec.Expr.Infix qualified as Infix (infixl1, infixr1, infixn1, prefix, postfix)

{-|
This combinator handles left-associative parsing, and the application of, 
__zero__ or more binary operators between __one__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ₋₁@, with left-associative application: 
fₙ₋₁(fₙ₋₂(..f₁(x₁, x₂).., xₙ₋₁), xₙ).
This application is then returned as the result of the combinator. 
If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.

See also 'Text.Gigaparsec.Expr.Infix.infixl1' for a version where the types can vary, 
ensuring that the associativity is enforced type-safely.
-}
chainl1 :: Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (a -> a -> a) -- ^ @op@, the operator between each value.
        -> Parsec a             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@, 
                                -- and applies their results left-associatively.
chainl1 = Infix.infixl1 id

{-|
This combinator handles right-associative parsing, and the application of, 
__zero__ or more binary operators between __one__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ@, with right-associative application: 
f₁(x₁,f₂(x₂,...fₙ₋₁(xₙ₋₁, xₙ)..)).
This application is then returned as the result of the combinator. 
If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.

See also 'Text.Gigaparsec.Expr.Infix.infixr1' for a version where the types can vary, 
ensuring that the associativity is enforced type-safely.
-}
chainr1 :: Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (a -> a -> a) -- ^ @op@, the operator between each value.
        -> Parsec a             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@,
                                --  and applies their results right-associatively.
chainr1 = Infix.infixr1 id

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
chainn1 :: Parsec a             -- ^ @p@, the value to be parsed
        -> Parsec (a -> a -> a) -- ^ @op@, the operator between each value.
        -> Parsec a             -- ^ a parser that parses @p@, and possibly an @op@ and then @p@, and applies the 
                                -- result of @op@ to those of @p@ in the same order.
chainn1 = Infix.infixn1 id

{-|
This combinator handles right-assocative parsing, and application of, 
__zero__ or more prefix unary operators to a single value.

First parse many repeated @op@s. 
When there are no more @op@s left to parse, parse a single @p@. 
The result of @p@, @x@, is applied to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @fₙ@ is applied first and @f₁@ last: @f₁ (f₂ (..(fₙ x)..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
prefix :: Parsec (a -> a) -- ^ @op@, the prefix operator to repeatedly parse before @p@.
       -> Parsec a        -- ^ @p@, the single value to be parsed
       -> Parsec a        -- ^ a parser that parses many @op@s, and a final @p@, and applies all 
                          -- of the results right-associatively.
prefix = Infix.prefix id

{-|
This combinator handles left-assocative parsing, and application of, 
__zero__ or more postfix unary operators to a single value.

First parse a single @p@.
Then, parse many repeated @op@s.
The result of @p@, @x@, is applied to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @f₁@ is applied first and @fₙ@ last: @fₙ(fₙ₋₁ (..(f₁ x)..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
postfix :: Parsec a        -- ^ @p@, the single value to be parsed
        -> Parsec (a -> a) -- ^ @op@, the postfix operator to repeatedly parse after @p@.
        -> Parsec a        -- ^ a parser that parses many @op@s, and a final @p@, and applies all 
                           -- of the results left-associatively.
postfix = Infix.postfix id

{-|
This combinator handles left-assocative parsing, and application of, 
__one__ or more postfix unary operators to a single value.

First parse a single @p@.
Then, parse at least one repeated @op@s.
The result of @p@, @x@, is applied first to @wrap@, and then to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @f₁@ is applied first and @fₙ@ last: @fₙ(fₙ₋₁ (..(f₁ (wrap x))..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
prefix1 :: (b -> a)             -- ^ @wrap@, a function converting @b@s into @a@s
        -> Parsec (a -> b)      -- ^ @op@, the prefix operator to repeatedly parse before @p@
        -> Parsec a             -- ^ @p@, the single value to be parsed
        -> Parsec b             -- ^ a parser that parses at least one @op@, and a final @p@,
                                -- and applies their results right-associatively.
prefix1 wrap op p = op <*> prefix ((wrap .) <$> op) p

{-|
This combinator handles left-assocative parsing, and application of, 
__one__ or more postfix unary operators to a single value.

First parse a single @p@.
Then, parse at least one repeated @op@s.
The result of @p@, @x@, is applied first to @wrap@, and then to each of the results of the @op@s, @f₁@ through @fₙ@, 
such that @f₁@ is applied first and @fₙ@ last: @fₙ( fₙ₋₁(..f₁ (wrap x)..))@. 
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
-}
postfix1 :: (b -> a)        -- ^ a function converting the value type @a@ into the result type @b@ 
         -> Parsec a        -- ^ @p@, the single value to be parsed
         -> Parsec (a -> b) -- ^ @op@, the postfix operator to repeatedly parse after @p@.
         -> Parsec b        -- ^ a parser that parses at least one @op@, and a final @p@, and applies all 
                            -- of the results left-associatively.
postfix1 wrap p op = postfix (p <**> op) ((. wrap) <$> op)

{-|
This combinator handles left-associative parsing, and the application of, 
__zero__ or more binary operators between __zero__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ₋₁@, with left-associative application: 
fₙ₋₁(fₙ₋₂(..f₁(x₁, x₂).., xₙ₋₁), xₙ).
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
If no @p@ could be parsed, return a default value @x@.
-}
chainl :: Parsec a             -- ^ @p@, the value to be parsed
       -> Parsec (a -> a -> a) -- ^ @op@, the operator between each value.
       -> a                    -- ^ @x@, the default value to return if no @p@s can be parsed.
       -> Parsec a             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@, 
                               -- and applies their results left-associatively.
                               -- If no @p@ was parsed, returns @x@.
chainl p op x = chainl1 p op <|> pure x

{-|
This combinator handles right-associative parsing, and the application of, 
__zero__ or more binary operators between __zero__ or more values.

First parse @p@, then parse @op@ followed by a @p@ repeatedly. 
The results of the @p@s, @x₁@ through @xₙ@, are combined with the results of the @op@s, 
@f₁@ through @fₙ@, with right-associative application: 
@f₁ x₁ (f₂ x₂ (...(fₙ₋₁ xₙ₋₁ xₙ)...))@.
This application is then returned as the result of the combinator. 

If @p@ or @op@ fails having consumed input at any point, the whole combinator fails.
If no @p@ could be parsed, return a default value @x@.

-}
chainr :: Parsec a             -- ^ @p@, the value to be parsed 
       -> Parsec (a -> a -> a) -- ^ @op@, the operator between each value. 
       -> a                    -- ^ @x@, the default value to return if no @p@s can be parsed.
       -> Parsec a             -- ^ a parser that parses alternating @p@ and @op@, ending in a @p@, 
                               -- and applies their results right-associatively.
                               -- If no @p@ was parsed, returns @x@.
chainr p op x = chainr1 p op <|> pure x
