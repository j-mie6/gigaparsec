{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-|
Module      : Text.Gigaparsec.Errors.Combinator
Description : This module contains combinators that can be used to directly influence error
              messages of parsers.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

Error messages are, by default, not /particularly/ descriptive. However, the combinators in this
module can be used to improve the generation of error messages by providing labels for expected
items, explanations for why things went wrong, custom error messages, custom unexpected error messages,
as well as correcting the offsets that error messages actually occurred at.

@since 0.2.0.0
-}
module Text.Gigaparsec.Errors.Combinator (
  -- * Error Enrichment Combinators
  -- | These combinators add additional information - or refine the existing information within - to
  -- an error message that has been generated within the scope of the parser they have been called on.
  -- These are a very basic, but effective, way of improving the quality of error messages generated
  -- by gigaparsec.
    label, (<?>), hide, explain,
  -- * Failure Combinators
  -- | These combinators immediately fail the parser, with a more bespoke message.
    emptyWide,
    fail, failWide,
    unexpected, unexpectedWide,
  -- * Error Adjustment Combinators
  -- | These combinators can affect at what position an error is caused at. They are
  -- opposites: where 'amend' will ensure an error message is said to have generated
  -- at the position on entry to the combinator, 'entrench' will resist these changes.
    amend, partialAmend, entrench, dislodge, dislodgeBy,
    amendThenDislodge, amendThenDislodgeBy, partialAmendThenDislodge, partialAmendThenDislodgeBy,
    markAsToken
  ) where

{-
Future doc headings:

Filtering Combinators
=====================
These combinators perform filtering on a parser, with particular emphasis on generating meaningful
error messages if the filtering fails. This is particularly useful for data validation within the
parser, as very instructive error messages describing what went wrong can be generated. These combinators
often filter using a `PartialFunction`: this may be because they combine filtering with mapping (in which
case, the error message is provided separately), or the function may produce a `String`.

In these cases, the partial function is producing the error messages: if the input to the function is
defined, this means that it is invalid and the filtering will fail using the message obtained from the
successful partial function invocation.

Generic Filtering Combinators
=============================
This combinators generalise the combinators from above, which are all special cases of them. Each of these
takes the characteristic predicate or function of the regular variants, but takes an `errGen` object that
can be used to fine-tune the error messages. These offer some flexiblity not offered by the specialised
filtering combinators, but are a little more verbose to use.

Important terminology to put in:

*a parser is said to /observably/ consume input when error messages generated by a parser @p@ occur at a deeper
offset than @p@ originally started at. While this sounds like it is the same as "having consumed input" for the
purposes of backtracking, they are disjoint concepts:

1. in @atomic p@, @p@ can /observably/ consume input even though the wider parser does not consume input due to the @atomic@.
1. in @amend(p)@, @p@ can consume input and may not backtrack even though the consumption is not /observable/ in the error
   message due to the @amend@.
-}

import Prelude hiding (fail)

import Text.Gigaparsec (Parsec)
-- We want to use this to make the docs point to the right definition for users.
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), line, col, emptyErr, specialisedErr, raise, unexpectedErr, hints, consumed, useHints, adjustErr, hints, hintsValidOffset)
import Text.Gigaparsec.Internal.Errors (ParseError, CaretWidth(FlexibleCaret, RigidCaret), ExpectItem(ExpectNamed))
import Text.Gigaparsec.Internal.Errors qualified as Internal (setLexical, amendErr, entrenchErr, dislodgeErr, partialAmendErr, labelErr, explainErr)
import Text.Gigaparsec.Internal.Require (require)

import Data.Set (Set)
import Data.Set qualified as Set (empty, map)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)

{-|
This combinator changes the expected component of any errors generated by this parser.

When this parser fails having not /observably/* consumed input, the expected component of the generated
error message is set to be the given items.
-}
label :: Set String -- ^ the names to give to the expected component of any qualifying errors.
      -> Parsec a   -- ^ the parser to apply the labels to
      -> Parsec a
label ls (Internal.Parsec p) =
  require (not (null ls) && not (any null ls)) "Text.Gigaparsec.Errors.Combinator.label"
                                               "labels cannot be empty" $
    Internal.Parsec $ \st good bad ->
      let !origConsumed = Internal.consumed st
          good' x st'
            | Internal.consumed st' /= origConsumed = good x st'
            | otherwise = good x st' { Internal.hints = Set.map ExpectNamed ls }
          bad' err = Internal.useHints bad (Internal.labelErr origConsumed ls err)
      in p st good' bad'

{-|
This combinator suppresses the entire error message generated by a given parser.

When this parser fails having not /observably/* consumed input, this combinator
replaces any error generated by the given parser to match the 'Text.Gigaparsec.empty' combinator.

This can be useful, say, for hiding whitespace labels, which are not normally useful
information to include in an error message for whitespace insensitive grammars.
-}
hide :: Parsec a -> Parsec a
hide (Internal.Parsec p) =
  Internal.Parsec $ \st good bad ->
    --FIXME: need to change to make it observable only
    p st (\x st' -> good x st' {Internal.hints = Set.empty})
         (\_ st' -> Internal.useHints bad (Internal.emptyErr st' 0) st')

{-|
This combinator adds a reason to error messages generated by this parser.

When this parser fails having not /observably/* consumed input, this combinator adds
a reason to the error message, which should justify why the error occured. Unlike error
labels, which may persist if more progress is made having not consumed input, reasons
are not carried forward in the error message, and are lost.
-}
explain :: String   -- ^ reason the reason why a parser failed.
        -> Parsec a -- ^ the parser to apply the reason to
        -> Parsec a
explain reason (Internal.Parsec p) =
  Internal.Parsec $ \st good bad ->
    let !origConsumed = Internal.consumed st
        bad' err = Internal.useHints bad (Internal.explainErr origConsumed reason err)
    in p st good bad'

{-|
This combinator fails immediately, with a caret of the given width and no other information.

By producing basically no information, this combinator is principally for adjusting the
caret-width of another error, rather than the value 'Text.Gigaparsec.empty', which is used to fail with
no effect on error content.
-}
emptyWide :: Word     -- ^ the width of the caret for the error produced by this combinator.
          -> Parsec a
emptyWide width = Internal.raise (`Internal.emptyErr` width)

{-|
This combinator consumes no input and fails immediately with the given error messages.

Produces a /specialised/ error message where all the lines of the error are the
given @msgs@ in order of appearance.

==== __Examples__
>>> let failing = fail ["hello,", "this is an error message", "broken across multiple lines"]

-}
fail :: NonEmpty String -- ^ the messages that will make up the error message.
     -> Parsec a
fail = _fail (FlexibleCaret 1)

{-|
This combinator consumes no input and fails immediately with the given error messages.

Produces a /specialised/ error message where all the lines of the error are the
given @msgs@ in order of appearance. The caret width of the message is set to the
given value.

==== __Examples__
>>> let failing = fail 3 ["hello,", "this is an error message", "broken across multiple lines"]

-}
failWide :: Word            -- ^ the width of the caret for the error produced by this combinator.
         -> NonEmpty String -- ^ the messages that will make up the error message.
         -> Parsec a
failWide width = _fail (RigidCaret width)

{-# INLINE _fail #-}
_fail :: CaretWidth -> NonEmpty String -> Parsec a
_fail width msgs = Internal.raise (\st -> Internal.specialisedErr st (NonEmpty.toList msgs) width)

unexpected :: String -> Parsec a
unexpected = _unexpected (FlexibleCaret 1)

unexpectedWide :: Word -> String -> Parsec a
unexpectedWide width = _unexpected (RigidCaret width)

{-# INLINE _unexpected #-}
_unexpected :: CaretWidth -> String -> Parsec a
_unexpected width name = Internal.raise $ \st -> Internal.unexpectedErr st Set.empty name width

amend :: Parsec a -> Parsec a
amend = _amend Internal.amendErr

partialAmend :: Parsec a -> Parsec a
partialAmend = _amend Internal.partialAmendErr

{-# INLINE _amend #-}
_amend :: (Word -> Word -> Word -> ParseError -> ParseError) -> Parsec a -> Parsec a
_amend f (Internal.Parsec p) =
  Internal.Parsec $ \st good bad ->
    let !origConsumed = Internal.consumed st
        !origLine = Internal.line st
        !origCol = Internal.col st
        !origHints = Internal.hints st
        !origHintsValidOffset = Internal.hintsValidOffset st
    in p st good $ \err st' -> bad (f origConsumed origLine origCol err)
                                   st' { Internal.hints = origHints
                                       , Internal.hintsValidOffset = origHintsValidOffset }

entrench :: Parsec a -> Parsec a
entrench = Internal.adjustErr Internal.entrenchErr

dislodge :: Parsec a -> Parsec a
dislodge = dislodgeBy maxBound

dislodgeBy :: Word -> Parsec a -> Parsec a
dislodgeBy by = Internal.adjustErr (Internal.dislodgeErr by)

amendThenDislodge :: Parsec a -> Parsec a
amendThenDislodge = dislodge . amend

amendThenDislodgeBy :: Word -> Parsec a -> Parsec a
amendThenDislodgeBy n = dislodgeBy n . amend

partialAmendThenDislodge :: Parsec a -> Parsec a
partialAmendThenDislodge = dislodge . partialAmend

partialAmendThenDislodgeBy :: Word -> Parsec a -> Parsec a
partialAmendThenDislodgeBy n = dislodgeBy n . partialAmend

markAsToken :: Parsec a -> Parsec a
markAsToken = Internal.adjustErr Internal.setLexical

{-|
This combinator changes the expected component of any errors generated by this parser.

This is just an alias for the 'label' combinator.
-}
{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: Parsec a -> Set String -> Parsec a
(<?>) = flip label
