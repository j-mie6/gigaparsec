{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Errors.Combinator (
    label, (<?>), hide,
    emptyWide,
    fail, failWide,
    unexpected, unexpectedWide,
    amend, partialAmend, entrench, dislodge, dislodgeBy,
    amendThenDislodge, amendThenDislodgeBy, partialAmendThenDislodge, partialAmendThenDislodgeBy,
    markAsToken
  ) where

import Prelude hiding (fail)

import Text.Gigaparsec (Parsec)
-- We want to use this to make the docs point to the right definition for users.
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), emptyErr, specialisedErr, raise, unexpectedErr, hints, consumed, useHints)
import Text.Gigaparsec.Internal.Errors (CaretWidth(FlexibleCaret, RigidCaret), ExpectItem(ExpectNamed), labelErr)
import Text.Gigaparsec.Internal.Require (require)

import Data.Set (Set)
import Data.Set qualified as Set (empty, map)

-- the empty set is weird here, do we require non-empty or just make it id?
label :: Set String -> Parsec a -> Parsec a
label ls (Internal.Parsec p) =
  require (not (null ls) && not (any null ls)) "Text.Gigaparsec.Errors.Combinator.label"
                                               "labels cannot be empty" $
    Internal.Parsec $ \st good bad ->
      let !origConsumed = Internal.consumed st
          good' x st'
            | Internal.consumed st' /= origConsumed = good x st'
            | otherwise = good x st'{Internal.hints = Set.map ExpectNamed ls}
          bad' err = Internal.useHints bad (labelErr origConsumed ls err)
      in p st good' bad'

hide :: Parsec a -> Parsec a
hide (Internal.Parsec p) =
  Internal.Parsec $ \st good bad ->
    p st (\x st' -> good x (st' {Internal.hints = Set.empty}))
         -- FIXME: parsley doesn't use the hints for hiding, could this be bug?
         (\_ st' -> {-Internal.useHints-} bad (Internal.emptyErr st' 0) st')

emptyWide :: Word -> Parsec a
emptyWide width = Internal.raise (`Internal.emptyErr` width)

fail :: [String] -> Parsec a
fail = _fail "Text.Gigaparsec.Errors.Combinator.fail" (FlexibleCaret 1)

failWide :: Word -> [String] -> Parsec a
failWide width = _fail "Text.Gigaparsec.Errors.Combinator.failWide" (RigidCaret width)

_fail :: String -> CaretWidth -> [String] -> Parsec a
_fail name width msgs =
  require (not (null msgs)) name "messages cannot be empty" $
    Internal.raise (\st -> Internal.specialisedErr st msgs width)

unexpected :: String -> Parsec a
unexpected = _unexpected (FlexibleCaret 1)

unexpectedWide :: Word -> String -> Parsec a
unexpectedWide width = _unexpected (RigidCaret width)

_unexpected :: CaretWidth -> String -> Parsec a
_unexpected width name = Internal.raise $ \st ->
  Internal.unexpectedErr st Set.empty name width

amend :: Parsec a -> Parsec a
amend = id --TODO:

partialAmend :: Parsec a -> Parsec a
partialAmend = id --TODO:

entrench :: Parsec a -> Parsec a
entrench = id --TODO:

dislodge :: Parsec a -> Parsec a
dislodge = dislodgeBy maxBound --TODO:

dislodgeBy :: Word -> Parsec a -> Parsec a
dislodgeBy _ = id --TODO:

amendThenDislodge :: Parsec a -> Parsec a
amendThenDislodge = dislodge . amend

amendThenDislodgeBy :: Word -> Parsec a -> Parsec a
amendThenDislodgeBy n = dislodgeBy n . amend

partialAmendThenDislodge :: Parsec a -> Parsec a
partialAmendThenDislodge = dislodge . partialAmend

partialAmendThenDislodgeBy :: Word -> Parsec a -> Parsec a
partialAmendThenDislodgeBy n = dislodgeBy n . partialAmend

markAsToken :: Parsec a -> Parsec a
markAsToken = id --TODO:

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a -> Set String -> Parsec a
(<?>) = flip label
