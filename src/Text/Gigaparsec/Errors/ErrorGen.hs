{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-|
Module      : Text.Gigaparsec.Expr
Description : This module can be used to generate hand-tuned error messages without using monadic bind.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module can be used to generate hand-tuned error messages without using monadic bind.

-}
module Text.Gigaparsec.Errors.ErrorGen (
    -- * Documentation
    -- ** Error Generators
    ErrorGen(..),
    UnexpectedItem(..), 
    -- *** Blank Generators
    vanillaGen, specializedGen,
    -- ** Error Generating Combinators
    {-|
    These combinators create parsers that fail or raise errors with messages desribed by a given 'ErrorGen'.
    -}
    asFail, asSelect, asErr, 
  ) where
import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), State, specialisedErr, emptyErr, expectedErr, unexpectedErr, raise)
import Text.Gigaparsec.Internal.Errors qualified as Internal (Error, CaretWidth(RigidCaret), addReason)

{-|
This type describes special primitives that can use the results of a previous parser 
to form and raise an error message. 
This is not something that is normally possible with raw combinators, without using '(>>=)', 
which is expensive.

Primarily, these are designed to be used with 
'Text.Gigaparsec.Errors.Combinator.filterSWith'\/'Text.Gigaparsec.Errors.Patterns.verifiedWith'\/'Text.Gigaparsec.Errors.Patterns.preventWith'
but can be used in other parsers as well. 
-}
type ErrorGen :: * -> *
data ErrorGen a 
  -- | An error generator for /Specialized/ errors, which can tune the freeform messages of the error.
  = SpecializedGen { 
      -- | Produces the messages of the error message when given the result of the offending parser. 
      messages :: a -> [String] -- FIXME: 0.3.0.0 change to NonEmptyList.
      -- | Controls how wide an error is based on the value @a@ and width @Word@ provided.
    , adjustWidth :: a -> Word -> Word
    }
  -- | An error generator for /Vanilla/ errors, which can tune the unexpected message and a generated reason.
  | VanillaGen { 
      -- | Produces the unexpected component (if any) of the error message when given the result of the offending parser.
      unexpected :: a -> UnexpectedItem
      -- | Produces the reason component (if any) of the error message when given the result of the offending parser.
    , reason :: a -> Maybe String
    , adjustWidth :: a -> Word -> Word
    }

-- | A blank /Vanilla/ error generator, which does not affect the unexpected message or reason.
vanillaGen :: ErrorGen a
vanillaGen = VanillaGen { unexpected = const EmptyItem
                        , reason = const Nothing
                        , adjustWidth = const id
                        }

-- | The default /Specialized/ error generator, which does not affect the error message.
specializedGen :: ErrorGen a
specializedGen = SpecializedGen { messages = const []
                                , adjustWidth = const id
                                }

{-|
This type describes how to form the unexpected component of a vanilla error message from a 'VanillaGen'.

This includes the different sorts of 'unexpected item' messages that may occur;
whether to display the expected characters, a name for the expected expression, or not to display at all.
-}
type UnexpectedItem :: *
data UnexpectedItem 
  -- | The error should use whatever input was consumed by the offending parser, verbatim.
  = RawItem 
  -- | The error should not have an unexpected component at all (as in 'Text.Gigaparsec.filterS').
  | EmptyItem 
  -- | The error should use the given name as the unexpected component.
  | NamedItem String

{-|
Given a parser result and its width, raise an error according to the given error generator.
-}
asErr :: ErrorGen a -- ^ @errGen@, the generator for the error message to raise.
      -> a          -- ^ @x@, the result of the offending parser
      -> Word       -- ^ The width of the parsed result, @x@.
      -> Parsec b   -- ^ A parser that unconditionally raises an error described by @errGen@.
asErr errGen x w = Internal.raise $ \st -> genErr errGen st x w

{-|
This combinator takes a given parser @p@ and unconditionally fails with a message based on @p@'s results.
-}
asFail  :: ErrorGen a       -- ^ @errGen@, the generator for the error message.
        -> Parsec (a, Word) -- ^ @p@, a parser that returns a result @x@ and its width @w@.
        -> Parsec b         -- ^ A parser that unconditionally fails with a message described by @errGen@, 
                            --   using the result of @p@.
asFail errGen (Internal.Parsec p) = Internal.Parsec $ \st _ bad ->
  let good (x, w) st' = bad (genErr errGen st' x w) st'
  in  p st good bad

{-|
This combinator takes a given parser @p@ and, if @p@ returns an @a@, 
fails with a message based on this result.
-}
asSelect  :: ErrorGen a                  -- ^ @errGen@, the generator for the error message.
          -> Parsec (Either (a, Word) b) -- ^ @p@, a parser which may produce a bad result of type @a@
          -> Parsec b                    -- ^ A parser that fails if @p@ produces a bad result, 
                                         --   otherwise returns the result of @p@ if it is a @b@
asSelect errGen (Internal.Parsec p) = Internal.Parsec $ \st good bad ->
  let good' (Right x) st' = good x st'
      good' (Left (x, w)) st' = bad (genErr errGen st' x w) st'
  in p st good' bad

genErr :: ErrorGen a -> Internal.State -> a -> Word -> Internal.Error
genErr SpecializedGen{..} st x w =
  Internal.specialisedErr st (messages x) (Internal.RigidCaret (adjustWidth x w))
genErr VanillaGen{..} st x w =
  addReason (reason x) (makeError (unexpected x) st (adjustWidth x w))

makeError :: UnexpectedItem -> Internal.State -> Word -> Internal.Error
makeError RawItem st cw = Internal.expectedErr st [] cw
makeError EmptyItem st cw = Internal.emptyErr st cw
makeError (NamedItem name) st cw = Internal.unexpectedErr st [] name (Internal.RigidCaret cw)

-- no fold, unlifed type
addReason :: Maybe String -> Internal.Error -> Internal.Error
addReason Nothing err = err
addReason (Just reason) err = Internal.addReason reason err
