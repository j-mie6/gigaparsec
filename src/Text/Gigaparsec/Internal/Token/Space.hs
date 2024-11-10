{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BlockArguments #-}

module Text.Gigaparsec.Internal.Token.Space (
  Space, whiteSpace, skipComments, alter, initSpace, mkSpace
  ) where

import Text.Gigaparsec (Parsec, eof, void, empty, (<|>), atomic, unit)
import Text.Gigaparsec.Char (satisfy, string, item, endOfLine)


import Text.Gigaparsec.Combinator (skipMany, skipManyTill)
import Text.Gigaparsec.Errors.Combinator (hide)

import Text.Gigaparsec.Token.Descriptions qualified as Desc
import Text.Gigaparsec.Token.Errors (
      ErrorConfig(labelSpaceEndOfLineComment,
                  labelSpaceEndOfMultiComment) )
import Text.Gigaparsec.Internal ( Parsec(Parsec) )
import Text.Gigaparsec.Internal.Token.Errors (annotate)

import Text.Gigaparsec.Internal.Require (require)


import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IORef (newIORef, IORef, readIORef, atomicModifyIORef)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (throw)
import Control.Monad (join, guard)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.RT.Unsafe (unsafeIOToRT)
import Control.Exception (Exception)

{-|
This type is concerned with special treatment of whitespace.

For the vast majority of cases, the functionality within this object shouldn't be needed, 
as whitespace is consistently handled by lexeme and fully. 
However, for grammars where whitespace is significant (like indentation-sensitive languages), 
this object provides some more fine-grained control over how whitespace is consumed by the parsers within lexeme.
-}
type Space :: *
data Space = Space {
  {-|
  Skips zero or more (insignificant) whitespace characters as well as comments.

  The implementation of this parser depends on whether 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true: 
  when it is, this parser may change based on the use of the alter combinator. 

  This parser will always use the hide combinator as to not appear as a valid alternative in an error message: 
  it's likely always the case whitespace can be added at any given time, but that doesn't make it a useful suggestion unless it is significant.
  -}
    whiteSpace :: !(Parsec ())
  {-|
  Skips zero or more comments.

  The implementation of this combinator does not vary with 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent'. 
  It will use the hide combinator as to not appear as a valid alternative in an error message: 
  adding a comment is often legal, 
  but not a useful solution for how to make the input syntactically valid.
  -}
  , skipComments :: !(Parsec ())
  {-|
  This combinator changes how lexemes parse whitespace for the duration of a given parser.

  So long as 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true, 
  this combinator will be able to locally change the definition of whitespace during the given parser.

  === __Examples__
  * In indentation sensitive languages, the indentation sensitivity is often ignored within parentheses or braces. 
  In these cases, 
  @parens (alter withNewLine p)@ 
  would allow unrestricted newlines within parentheses.
  -}
  , alter :: forall a. Desc.CharPredicate -> Parsec a -> Parsec a
  {-|
  This parser initialises the whitespace used by the lexer when 
  'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true.

  The whitespace is set to the implementation given by the lexical description.
  This parser must be used, by fully or otherwise, 
  as the first thing the global parser does or an UnfilledRegisterException will occur.

  See 'alter' for how to change whitespace during a parse.
  -}
  , initSpace :: Parsec ()
  }

---------------------------------------------------------------------------------------------------
-- Whitespace parsers setup

{-|
A global ref to the mapping from threads to their corresponding whitespace parsers.

Implemented in terms of `unsafePerformIO` -- use with caution!
-}
-- TODO: use a HashMap instead of a Map?
{-# NOINLINE wsImplMap #-}
wsImplMap :: IORef (Map ThreadId (Parsec ()))
wsImplMap = unsafePerformIO (newIORef Map.empty)


{-| 
Get the whitespace parser for this thread.

This parser consumes no input and always succeeds.
Throws a ghc `error` if the whitespace parser has not been initialised in `wsImplMap`
-}
getWs :: Parsec (Parsec ())
getWs = Parsec $ \st good _ ->
  do  ws <- unsafeIOToRT (Map.lookup <$> myThreadId <*> readIORef wsImplMap)
      case ws of
        Just p  -> good p st
        Nothing -> error "Gigaparsec.Internal.Token.Lexer.getWs: whitespace parser not initialised."

{-|
Atomically replace the whitespace parser for this thread.

This parser consumes no input and always succeeds.
-}
setWs :: Parsec () -- ^ @ws@, the new whitespace parser for this thread
      -> Parsec () -- ^ 
setWs ws = Parsec $ \st good _ ->
  do  unsafeIOToRT (
        do  tid <- myThreadId
            atomicModifyIORef wsImplMap (\m -> (Map.insert tid ws m, ())))
      good () st

{-|
Run a parser and, if it fails __without consuming input__, undo its modifications to the 
current thread's whitespace parser.

This parser consumes input only if @p@ does also; 
it fails if and only if @p@ fails __having consumed input__.
-}
rollbackWs  :: Parsec a -- ^ @p@, the parser to run
            -> Parsec a -- ^ a parser that runs @p@, and restores the original value of this 
                        -- thread's whitespace parser if @p@ fails without consuming input.
rollbackWs p = do
  ws <- getWs
  p <|> (setWs ws *> empty)

{-|
Run the given parser @p@ with a new whitespace parser, and then reset this value if @p@
succeeds.

Behaves like 'set', except the scope of the update of the whitespace parser is limited just to the 
given parser @p@, assuming that @p@ succeeds.
This parser consumes input and fails if and only if the given parser @p@ does also.
-}
setWsDuring :: Parsec () -- ^ @ws@, the new temporary whitespace parser
            -> Parsec a  -- ^ @p@, the parser to run with the modified whitespace parser
            -> Parsec a  -- ^ a parser which runs @p@ with the new whitespace parser, 
                         -- and resets the old whitespace parser if @p@ succeeds.
setWsDuring ws p = do
  oldWs <- getWs
  setWs ws
  p <* setWs oldWs

mkSpace :: Desc.SpaceDesc -> ErrorConfig -> Space
mkSpace desc@Desc.SpaceDesc{..} !errConfig = Space {..}
  where -- don't think we can trust doing initialisation here, it'll happen in some random order

        -- This is the global ref which holds the whitespace implementation for parsers where 
        -- 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true.
        !wsMap = wsImplMap
        comment = commentParser desc -- do not make this strict
        implOf
          | supportsComments desc = hide . maybe skipComments (skipMany . (<|> comment errConfig) . void . satisfy)
          | otherwise             = hide . maybe empty (skipMany . satisfy)
        !configuredWhitespace = implOf space
        !whiteSpace
          | whitespaceIsContextDependent = join getWs
          | otherwise                    = configuredWhitespace
        !skipComments = skipMany (comment errConfig)
        alter p
          | whitespaceIsContextDependent = rollbackWs . setWsDuring (implOf p)
          | otherwise                    = throw (UnsupportedOperation badAlter)
        initSpace -- Initialise the whitespace implementation
          | whitespaceIsContextDependent = setWs configuredWhitespace
          | otherwise                    = throw (UnsupportedOperation badInit)
        badInit = "whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is True"
        badAlter = "whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is True"

---------------------------------------------------------------------------------------------------
-- Comment Parsing

{-
We have the following invariances to be checked up front:
  * at least one kind of comment must be enabled
  * the starts of line and multiline must not overlap

-- TODO: needs error messages put in (is the hide correct)
-- TODO: remove guard, configure properly
-}
commentParser :: Desc.SpaceDesc -> ErrorConfig -> Parsec ()
commentParser Desc.SpaceDesc{..} !errConfig =
  require (multiEnabled || singleEnabled) "skipComments" noComments $
    require (not (multiEnabled && isPrefixOf multiLineCommentStart lineCommentStart)) "skipComments" noOverlap $
      hide (multiLine <|> singleLine)
  where
    -- can't make these strict until guard is gone
    openComment = atomic (string multiLineCommentStart)
    closeComment = annotate (labelSpaceEndOfMultiComment errConfig) (atomic (string multiLineCommentEnd))
    multiLine = guard multiEnabled *> openComment *> wellNested 1
    wellNested :: Int -> Parsec ()
    wellNested 0 = unit
    wellNested n = closeComment *> wellNested (n - 1)
               <|> guard multiLineNestedComments *> openComment *> wellNested (n + 1)
               <|> item *> wellNested n
    singleLine = guard singleEnabled
              *> atomic (string lineCommentStart)
              *> skipManyTill item (annotate (labelSpaceEndOfLineComment errConfig) endOfLineComment)

    endOfLineComment
      | lineCommentAllowsEOF = void endOfLine <|> eof
      | otherwise            = void endOfLine

    multiEnabled = not (null multiLineCommentStart || null multiLineCommentEnd)
    singleEnabled = not (null lineCommentStart)
    noComments = "one of single- or multi-line comments must be enabled"
    noOverlap = "single-line comments must not overlap with multi-line comments"

supportsComments :: Desc.SpaceDesc -> Bool
supportsComments Desc.SpaceDesc{..} = not (null lineCommentStart && null multiLineCommentStart)

type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation
