{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Text.Gigaparsec.Errors.Combinator (
    label, (<?>),
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
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), labelErr, emptyErr, specialisedErr, raise, unexpectedErr)
import Text.Gigaparsec.Internal.Errors (CaretWidth(FlexibleCaret, RigidCaret))
import Text.Gigaparsec.Internal.Require (require)

import Data.Set (Set)
import Data.Set qualified as Set (empty)

-- the empty set is weird here, do we require non-empty or just make it id?
label :: Set String -> Parsec a -> Parsec a
label ls (Internal.Parsec p) =
  require (not (any null ls)) "Text.Gigaparsec.Errors.Combinator.label" "labels cannot be empty" $
    -- TODO: hints on good!
    Internal.Parsec $ \st good bad -> p st good (\err st' -> bad (Internal.labelErr st' ls err) st')

emptyWide :: Word -> Parsec a
emptyWide width = Internal.raise (`Internal.emptyErr` width)

fail :: [String] -> Parsec a
fail = _fail "Text.Gigaparsec.Errors.Combinator.fail" (FlexibleCaret 1)

failWide :: Word -> [String] -> Parsec a
failWide = _fail "Text.Gigaparsec.Errors.Combinator.failWide" . RigidCaret

_fail :: String -> CaretWidth -> [String] -> Parsec a
_fail name width msgs =
  require (not (null msgs)) name "messages cannot be empty" $
    Internal.raise (\st -> Internal.specialisedErr st msgs width)

unexpected :: String -> Parsec a
unexpected = _unexpected (FlexibleCaret 1)

unexpectedWide :: Word -> String -> Parsec a
unexpectedWide = _unexpected . RigidCaret

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
