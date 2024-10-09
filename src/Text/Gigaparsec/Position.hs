{-# LANGUAGE Safe #-}
{-|
Module      : Text.Gigaparsec.Position
Description : This module contains parsers that provide a way to extract position information during parsing.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : stable

This module contains parsers that provide a way to extract position information during parsing.

Position parsers can be important for when the final result of the parser needs to encode position information for later consumption: 
this is particularly useful for abstract syntax trees. 
Offset is also exposed by this interface (via 'offset'), which may be useful for establishing a caret size in specialised error messages.
-}
module Text.Gigaparsec.Position (
  {-| === Position
  The position of a parser determines where in the input stream the next character is to be consumed.
  This position is determined by the line (see 'line') and column (see 'col') numbers at which the character occurs in the input text.

  To simply see how many characters total have been parsed, see 'offset'.
  -}
  Pos, 
  line, 
  col, 
  pos, 
  {-| === Offset & Width
  'offset' and 'withWidth' allow one to determine how much input has been consumed by a parser.
  -}
  offset, 
  withWidth) where

import Text.Gigaparsec.Internal (Parsec)
import Text.Gigaparsec.Internal qualified as Internal (Parsec(Parsec), line, col, consumed)
import Control.Applicative (liftA2, liftA3)


{-|
A type representing the position of a parser in terms of line and column numbers.
-}
type Pos :: *
type Pos = (Word, Word)

{-|
This parser returns the current line number (starting at 1) of the input without having any other effect.

When this combinator is ran, no input is required, nor consumed, and the current line number will always be successfully returned. 
It has no other effect on the state of the parser.

@since 0.3.0.0
-}
line :: Parsec Word -- ^ a parser that returns the line number the parser is currently at.
line = Internal.Parsec $ \st good _ -> good (Internal.line st) st

{-|
This parser returns the current column number (starting at 1) of the input without having any other effect.

When this combinator is run, no input is required, nor consumed, and the current column number will always be successfully returned. 
It has no other effect on the state of the parser.
@since 0.3.0.0
-}
col :: Parsec Word -- ^ a parser that returns the column number the parser is currently at.
col = Internal.Parsec $ \st good _ -> good (Internal.col st) st

{-|
This parser returns the current line and column numbers (starting at 1) of the input without having any other effect.

When this combinator is ran, no input is required, nor consumed, and the current line and column number will always be successfully returned. 
It has no other effect on the state of the parser.
@since 0.3.0.0
-}
pos :: Parsec Pos -- ^ a parser that returns the line and column number the parser is currently at.
pos = liftA2 (,) line col

{-|
This parser returns the current offset - the total number of characters consumed - into the input (starting at 0) without having any other effect.

When this combinator is ran, no input is required, nor consumed, and the current offset into the input will always be successfully returned. 
It has no other effect on the state of the parser.
-}
offset :: Parsec Word -- ^ a parser that returns the offset the parser is currently at.
offset = Internal.Parsec $ \st good _ -> good (Internal.consumed st) st

{-|
This combinator returns the result of a given parser @p@ and the number of characters it consumed.

First records the initial offset on entry to given parser @p@, then executes @p@. 
If @p@ succeeds, then the offset is taken again, and the two values are subtracted to give width @w@. 
The result, @x@, of @p@ is returned along with @w@ as the pair @(x, w)@. 
If @p@ fails, this combinator will also fail.
-}
withWidth :: Parsec a         -- ^ the parser whose width we wish to compute
          -> Parsec (a, Word) -- ^ a parser that pairs the result of the parser @p@ with the number of characters it consumed
withWidth p = liftA3 (\s x e -> (x, e-s)) offset p offset
