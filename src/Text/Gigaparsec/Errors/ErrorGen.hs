{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Text.Gigaparsec.Errors.ErrorGen (
    ErrorGen(..), UnexpectedItem(..), asFail, asSelect, asErr, vanillaGen, specializedGen
  ) where
import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), State, specialisedErr, emptyErr, expectedErr, unexpectedErr, raise)
import Text.Gigaparsec.Internal.Errors qualified as Internal (Error, CaretWidth(RigidCaret), addReason)

type ErrorGen :: * -> *
data ErrorGen a = SpecializedGen { messages :: a -> [String] -- FIXME: 0.3.0.0 change to NonEmptyList
                                 , adjustWidth :: a -> Word -> Word
                                 }
                | VanillaGen { unexpected :: a -> UnexpectedItem
                             , reason :: a -> Maybe String
                             , adjustWidth :: a -> Word -> Word
                             }

vanillaGen :: ErrorGen a
vanillaGen = VanillaGen { unexpected = const EmptyItem
                        , reason = const Nothing
                        , adjustWidth = const id
                        }

specializedGen :: ErrorGen a
specializedGen = SpecializedGen { messages = const []
                                , adjustWidth = const id
                                }

type UnexpectedItem :: *
data UnexpectedItem = RawItem | EmptyItem | NamedItem String

asErr :: ErrorGen a -> a -> Word -> Parsec b
asErr errGen x w = Internal.raise $ \st -> genErr errGen st x w

asFail :: ErrorGen a -> Parsec (a, Word) -> Parsec b
asFail errGen (Internal.Parsec p) = Internal.Parsec $ \st _ bad ->
  let good (x, w) st' = bad (genErr errGen st' x w) st'
  in  p st good bad

asSelect :: ErrorGen a -> Parsec (Either (a, Word) b) -> Parsec b
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
