{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE  BlockArguments, 
              MagicHash, 
              UnboxedTuples, 
              CPP, 
              ExistentialQuantification, 
              LambdaCase, 
              NamedFieldPuns, 
              UnliftedFFITypes 
              #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
#include "portable-unlifted.h"

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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map


import Data.IORef (newIORef, IORef, readIORef, atomicModifyIORef, atomicModifyIORef')
import Control.Concurrent (ThreadId, myThreadId, mkWeakThreadId)
import Control.Exception (throw)
import Control.Monad (join, guard, foldM, filterM)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.RT.Unsafe (unsafeIOToRT)
import Control.Exception (Exception)
import GHC.Weak (Weak, deRefWeak)
import GHC.Exts (Word64#, ThreadId#, unsafeCoerce#)

CPP_import_PortableUnlifted
import Data.Word (Word64)
import Data.Ref (Ref, readRef, writeRef, newRef)
import Control.Monad.RT (rtToIO)
import Control.Monad.RT (RT)
import Data.Maybe (isJust)
import Control.Arrow (Arrow(second))
import Data.Traversable (for)
import GHC.Conc (ThreadId(ThreadId))

---------------------------------------------------------------------------------------------------
-- We need a way to generate a hashable value that is unique to each thread.
-- GHC ≥ 9.8 provides `fromThreadId`.
-- Lower versions have all the mechanisms to create this function, but they keep these internal.
-- Thus, we have to do some naughty `foreign import`s :(

-- `fromThreadID` is only available in base ≥ 4.19
#if __GLASGOW_HASKELL__ >= 908
-- GHC >= 9.8
import GHC.Conc.Sync (fromThreadId)

#elif __GLASGOW_HASKELL__ >= 902 
-- GHC >= 9.2.1
-- base 4.17 - 4.18, `getThreadId` returns CULLong
import Foreign.C (CULLong(CULLong))

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CULLong

{-|
Map a thread to an integer identifier which is unique within the
current process.
-}
fromThreadId :: ThreadId -> Word64
fromThreadId (ThreadId tid) = fromIntegral (getThreadId tid)

#else
-- base 4.16 and below, `getThreadId` returns CInt
import Foreign.C (CInt(CInt))

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt

{-|
Map a thread to an integer identifier which is unique within the
current process.
-}
fromThreadId :: ThreadId -> Word64
fromThreadId (ThreadId tid) = fromIntegral (getThreadId tid)

#endif
---------------------------------------------------------------------------------------------------



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
-- Comment Parsing

{-|
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

{-|
One should not use `alter` or `initSpace` when white space is not context dependent,
else throw this error.
-}
type UnsupportedOperation :: *
newtype UnsupportedOperation = UnsupportedOperation String deriving stock Eq
instance Show UnsupportedOperation where
  show (UnsupportedOperation msg) = "unsupported operation: " ++ msg
instance Exception UnsupportedOperation


---------------------------------------------------------------------------------------------------
-- Whitespace parsers setup

mkSpace :: Desc.SpaceDesc -> ErrorConfig -> Space
mkSpace desc@Desc.SpaceDesc{..} !errConfig = Space {..}
  where
  -- don't think we can trust doing initialisation here, it'll happen in some random order
  -- This is the global ref which holds the whitespace implementation for parsers where 
  -- 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true.
  {-
  A per-lexer ref to the mapping from raw thread id hashes to their corresponding whitespace parsers 
  and a weak pointer to the thread.

  Implemented in terms of `unsafePerformIO` -- use with caution!
  -}
  {-# NOINLINE wsImplMap #-}
  wsImplMap :: WsMap
  !wsImplMap = unsafePerformIO (newIORef Map.empty)


  ~comment = commentParser desc -- do not make this strict
  implOf
    | supportsComments desc = hide . maybe skipComments (skipMany . (<|> comment errConfig) . void . satisfy)
    | otherwise             = hide . maybe empty (skipMany . satisfy)
  !configuredWhitespace = implOf space
  !whiteSpace
    | whitespaceIsContextDependent = join (getWs wsImplMap)
    | otherwise                    = configuredWhitespace
  !skipComments = skipMany (comment errConfig)
  alter p
    | whitespaceIsContextDependent = rollbackWs wsImplMap . setWsDuring wsImplMap (implOf p)
    | otherwise                    = throw (UnsupportedOperation badAlter)
  initSpace -- Initialise the whitespace implementation
    | whitespaceIsContextDependent = initWs wsImplMap configuredWhitespace
    | otherwise                    = throw (UnsupportedOperation badInit)
  badInit = "whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is True"
  badAlter = "whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is True"


---------------------------------------------------------------------------------------------------
-- Per-thread whitespace parser maps.
{-
To keep the changes to the whitespace parser thread-safe, we need to maintain a map associating
threads to their whitespace parsers.

Naively doing this with a @Map ThreadId (Parsec ())@ means dead threads cannot be garbage collected,
as they are still in the map.
Instead, we have *two* maps, each using a `Word64` key which is unique to each thread. 
Crucially, this key is not directly linked to the thread (in terms of pointers), 
meaning the latter can be garbage collected after dying, while still
having a representative in the map.

The unique `Word64` is generated by a naughty call to `fromThreadId`, which provides
a unique identifier for the thread.
We call this the 'raw thread id'.

1. `WkThreadMap`: 
  A mapping from the raw thread id to a *weak* pointer to the thread.
  The weak pointer means we are able to access the thread, but it does not prevent the thread from being
  garbage collected upon death.

  This does, however, mean we need to manually clean up the entries for dead threads in this map.
  We can query the weak pointer to determine if the thread is still alive, and remove it otherwise.

2. `RefMap Word64 (Ref Parsec())`:
  A mapping from raw thread ids to their current whitespace parsers.

  The whitespace parsers are implemented with a `Ref` which should only be accessed by the associated thread.
  We use a `Ref` so that the entry in the map does not need to be modified when the whitespace parser is updated,
  instead we mutate the value of the `Ref`.

  As with the `WkThreadMap`, we need to manually clean up entries of dead threads, using a similar method.
-}

{-| The 'raw' value of a `ThreadId`.

Obtained using `fromThreadId`.
-}
type RawThreadId = Word64

-- | A whitespace parser
type WsParser = Parsec ()

{-| Existential of a 'Ref' and its lifetime.

Essentially, 
> @`ERef` a = Σ (r : *) . `Ref` r a@
-}
data ERef a = forall r . ERef {-# UNPACK #-} !(Ref r a)

{-|
A pair of a weak pointer to a 'ThreadId', and a ref to the current whitespace parser for that thread.
-}
data WsPair = 
  WsPair 
    {-# UNPACK #-} !(Weak ThreadId)
    !(ERef WsParser) -- Can't unpack this as it is an existential.

{-| An `IORef` to a strict map of 'RawThreadId's to 'WsPair's. 

The 'WsPair' entries  consist of the whitespace parser for the associated thread
and a weak pointer to the thread's 'ThreadId'
-}
type WsMap = IORef (Map RawThreadId WsPair)


{-|
Get a weak pointer to the current thread.

Used in the `WsMap`.
-}
{-# INLINE myWeakThreadId #-}
myWeakThreadId :: IO (Weak ThreadId)
myWeakThreadId = mkWeakThreadId =<< myThreadId

{-|
Get a unique key (the `RawThreadId`) of the current thread.
This provides the 'raw thread id' (see above).
-}
{-# INLINE myRawThreadId #-}
myRawThreadId :: IO RawThreadId
myRawThreadId = fromThreadId <$>  myThreadId



{-|
This function both:

* Removes all dead threads from the `WsMap`, /and/
* Inserts the current thread and a whitespace parser in the `WsMap`.

This is achieved in one atomic-modify.
-}
{-
Some Notes:

### Race Cond
There is a harmless race condition between removing deads from `WkThreadMap` and
removing them from the `RefMap`.
The case:
1. Thread @X@ gets aliveThreads and deadThreads,
3. @Y@ pre-empts @X@, and gets aliveThreads, deadThreads
3. @Y@ removes deadThreads from rList, then also from rMap
4. @X@ wakes back up, and attempts to remove deadThreads from rMap.
    However, not all tids in deadThreads need be in rMap anymore.
    But this is fine, as calling delete on a key not present is just ignored.

### Why the expensive `refMapUpdate`
It is thread-safer to remove a list of dead threads than it is to replace the map with only alive threads.
That is, there is no harm in removing dead threads that are not in the map,
but there is harm in clobbering threads that were spawned between the `WkThreadMap` update and
that of the `RefMap`.
-}
updateThreadMaps :: WsMap -> WsParser -> RT ()
updateThreadMaps mp ws = newRef ws $ \ref -> unsafeIOToRT $ do 
  tid <- myRawThreadId
  wkTid <- myWeakThreadId
  atomicModifyIORef' mp (removeDeadsAndAddNewRef tid (WsPair wkTid (ERef ref)))
  where
    -- Remove all dead threads from the WsMap using `isAlive`,
    -- then add the new tid/val to the map.
    removeDeadsAndAddNewRef :: RawThreadId -> WsPair -> Map RawThreadId WsPair -> (Map RawThreadId WsPair, ())
    removeDeadsAndAddNewRef tid val xs = (Map.insert tid val (Map.filter isAlive xs), ())

    -- Determine if the thread is still alive.
    -- WARNING: defined with `unsafePerformIO`, use with caution.
    {-# NOINLINE isAlive #-}
    isAlive :: WsPair -> Bool
    isAlive (WsPair wkTid _) = unsafePerformIO $! (isJust <$>) $! deRefWeak wkTid


{-| 
Get the whitespace parser for this thread.

This parser consumes no input and always succeeds.
Throws a ghc `error` if the whitespace parser has not been initialised in the given map.
-}
{-# INLINE getWs #-}
getWs :: WsMap -> Parsec WsParser
getWs wsMap = Parsec $ \st good _ -> do
  ERef ref <- getMapRef wsMap
  (`good` st) =<< readRef ref

{-| 
Get the ref to the 'WsParser' for this thread.

Throws a ghc `error` if the ref is not in the given map.
-}
-- TODO: It should be an invariant that we have initialised the ref before
-- calling getMapRef, so that this function never fails.
{-# INLINE getMapRef #-}
getMapRef :: WsMap -> RT (ERef WsParser)
getMapRef wsMap = unsafeIOToRT $ do
  x <- Map.lookup <$> myRawThreadId <*> readIORef wsMap
  case x of
    Nothing -> error . errMsg <$> myRawThreadId
    Just (WsPair _ p)  -> return p
  where
    -- Error to display when the ref for this thread's wsparser has not been 
    -- initialised in the wsMap.
    errMsg :: RawThreadId -> String
    errMsg tid = concat [
        "\n > Gigaparsec Error: Whitespace parser has not been initialised for this thread."
      , "\n | This usually occurs if you have not used the `fully` combinator in `Text.Gigaparsec.Token.Lexer`."
      , "\n | `fully` handles the initialisation of the whitespace parsers given by a lexer description."
      , "\n | If you use the lexical descriptions, then any 'top-level' parser should be wrapped in the `fully` combinator."
      , "\n | This includes any parser that is given to, for example, the `parse`, `parseFromFile`, and `parseRepl` functions in `Text.Gigaparsec`"
      , "\n\n   (Internal Error): Text.Gigaparsec.Internal.Token.Space.getMapRef: "
      , "entry in `RefMap` not initialised for thread: "
      , show tid 
      ]

{-|
Replace the whitespace parser for this thread.

This has no race conditions as:

* the `Ref` entry in the map stays the same, instead the value of the `Ref` is modified, 
* threads only access their own associated `Ref`.

This parser consumes no input and always succeeds.
Throws a ghc `error` if this thread does not have an associated entry in the given map.
-}
{-# INLINE setWs #-}
setWs :: WsMap -- ^ `IORef` to the map of whitespace parsers
      -> WsParser -- ^ @ws@, the new whitespace parser for this thread
      -> Parsec () -- ^ parser that updates the wsparser to @ws@
setWs wsMap ws = Parsec $ \st good _ ->
  do  ERef ref <- getMapRef wsMap
      writeRef ref ws
      good () st

{-|
Initialise the `Ref` to the whitespace parser for this thread, with the given parser.

This has no race conditions as it uses `updateThreadMaps`, which atomically updates the `WsMap`.

This parser consumes no input and always succeeds.
-}
{-# INLINE initWs #-}
initWs :: WsMap     -- ^ IORef to a map of whitespace parser refs
       -> WsParser  -- ^ the initial whitespace parser for this thread.
       -> Parsec () -- ^ a parser that adds the new whitespace parser to the map, 
                    -- and cleans up any dead entries in the map (see `updateThreadMaps`).
initWs wsMap ws = Parsec $ \st good _ -> do
  updateThreadMaps wsMap ws
  good () st

{-|
Run a parser and, if it fails __without consuming input__, undo its modifications to the 
current thread's whitespace parser.

This parser consumes input only if @p@ does also; 
it fails if and only if @p@ fails __having consumed input__.
-}
{-# INLINE rollbackWs #-}
rollbackWs  :: WsMap
            -> Parsec a -- ^ @p@, the parser to run
            -> Parsec a -- ^ a parser that runs @p@, and restores the original value of this 
                        -- thread's whitespace parser if @p@ fails without consuming input.
rollbackWs wsMap p = do
  ws <- getWs wsMap
  p <|> (setWs wsMap ws *> empty)

{-|
Run the given parser @p@ with a new whitespace parser, and then reset this value if @p@
succeeds.

Behaves like 'set', except the scope of the update of the whitespace parser is limited just to the 
given parser @p@, assuming that @p@ succeeds.
This parser consumes input and fails if and only if the given parser @p@ does also.
-}
{-# INLINE setWsDuring #-}
setWsDuring :: WsMap
            -> WsParser -- ^ @ws@, the new temporary whitespace parser
            -> Parsec a -- ^ @p@, the parser to run with the modified whitespace parser
            -> Parsec a -- ^ a parser which runs @p@ with the new whitespace parser, 
                        -- and resets the old whitespace parser if @p@ succeeds.
setWsDuring wsMap ws p = do
  oldWs <- getWs wsMap
  setWs wsMap ws
  p <* setWs wsMap oldWs
