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


import Data.List (isPrefixOf, partition)
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
import GHC.Exts (Word64#, ThreadId#)

CPP_import_PortableUnlifted
import Data.Word (Word64)
import Data.Ref (Ref, readRef, writeRef, newRef)
import Control.Monad.RT (rtToIO)
import Control.Monad.RT (RT)
import Data.Maybe (isJust)
import Control.Arrow (Arrow(second))
import Data.Traversable (for)
import GHC.Conc (ThreadId(ThreadId))

-- `fromThreadID` is only available in base ≥ 4.19
#if __GLASGOW_HASKELL__ >= 908
import GHC.Conc (fromThreadID)
#else

import GHC.Prim(ThreadId#)
import Foreign.C (CULLong(CULLong))

-- Copied from
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/GHC-Conc-Sync.html#v:fromThreadId
{-|
Map a thread to an integer identifier which is unique within the
current process.
-}
fromThreadId :: ThreadId -> Word64
fromThreadId (ThreadId t) = fromIntegral $ getThreadId t

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CULLong

#endif

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

newtype WkThreadId = WkThreadId (Weak ThreadId)


type MapRef :: * -> * -> *
type MapRef k v = IORef (Map k v)

{-# INLINE myWeakThreadId #-}
myWeakThreadId :: IO (Weak ThreadId)
myWeakThreadId = mkWeakThreadId =<< myThreadId

myThreadIDKey :: IO Word64
myThreadIDKey = fromThreadId <$>  myThreadId



-- | A strict pair of `Word64` and @`Weak` `ThreadId`@s.
type StrictWTWord :: *
data StrictWTWord = 
  WTW 
    { unTID      :: {-# UNPACK #-} !Word64,
      unWkThread :: {-# UNPACK #-} !(Weak ThreadId)
    }

-- | Existential of a 'Ref' and its lifetime.
-- 
-- Essentially, 
-- > @`ERef` a = Σ (r : *) . `Ref` r a@
data ERef a = forall r . ERef {-# UNPACK #-} !(Ref r a)

-- | A map of `Word64` to its associated @`Weak` `ThreadId`@.
-- The `Word64` will be given by the thread's `ThreadId`.
type WkThreadMap = IORef [StrictWTWord]

{-
Generalised from:
https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.Data.OldList.html#deleteBy
-}
deleteBy                :: (b -> a -> Bool) -> b -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

{-
Generalised from:
https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.Data.OldList.html#deleteFirstsBy
-}
deleteFirstsBy          :: (b -> a -> Bool) -> [a] -> [b] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

{-|
This function both:

* Removes all dead threads from the `WkThreadMap` and `RefMap`, /and/
* Registers the current thread in the `WkThreadMap` and `RefMap`.

This is achieved in two atomic modifications:

* one atomic modification to remove dead threads and add this thread to the `WkThreadMap`
* one atomic modification to remove dead threads and add this thread to the `RefMap`

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


### Why not use partition
Notice we traverse the `WkThreadMap` twice; once to find `deadThreads`,
and the second in the `WkThreadMap` update, removing all dead threads.
If we were to use partition, (or partitionM), then naively we might think to just
atomically replace the WkThreadMap with `aliveThreads` (given by partition).
However, if we are pre-empted, and wake up later, `aliveThreads` may contain a thread that died while
we were asleep. In this case, we will have mistakenly added a dead thread back into the list.
-}
updateThreadMaps :: WkThreadMap -> RefMap Word64 v -> v -> RT ()
updateThreadMaps rList rMap ws = newRef ws $ \ref -> unsafeIOToRT $ do
  xs <- readIORef rList
  deadThreads <- map unTID <$> filterM (fmap isJust . deRefWeak . unWkThread) xs
  tid <- myThreadIDKey
  wkRef <- myWeakThreadId
  atomicModifyIORef' rList (tidMapUpdate deadThreads tid wkRef)
  atomicModifyIORef' rMap (refMapUpdate deadThreads tid (ERef ref))

  where
    -- How to update the (TID -> Ref) map.
    -- This removes all dead threads, and inserts this thread with the given ref.
    refMapUpdate :: [Word64] -> Word64 -> ERef v -> Map Word64 (ERef v) -> ((Map Word64 (ERef v)), ())
    refMapUpdate deadThreads thisThreadHash thisThreadRef mp = (
        let removedDeads = foldl (flip Map.delete) mp deadThreads
        in  Map.insert thisThreadHash thisThreadRef removedDeads
        , ()
      )
    -- How to update the (TID -> WkThreadId) map.
    -- this removes all dead threads, and inserts this thread with a weak ref to its Id.
    tidMapUpdate :: [Word64] -> Word64 -> Weak ThreadId -> [StrictWTWord] -> ([StrictWTWord], ())
    tidMapUpdate deadThreads tidHash wkRef threads = 
      (WTW tidHash wkRef : deleteFirstsBy (\x y -> x == unTID y) threads deadThreads, ())

    

-- | An `IORef` to a (strict) map of (strict) @k@s to (strict) `ERef`s of @v@s  
type RefMap k v = IORef (Map k (ERef v))

{-| 
Get the whitespace parser for this thread.

This parser consumes no input and always succeeds.
Throws a ghc `error` if the whitespace parser has not been initialised in `wsImplMap`
-}
{-# INLINE getWs #-}
getWs :: RefMap Word64 (Parsec ()) -> Parsec (Parsec ())
getWs wsMap = Parsec $ \st good _ -> do
  ERef ref <- getMapRef wsMap
  (`good` st) =<< readRef ref

{-| 
Get the ref to the @v@ for this thread.

Throws a ghc `error` if the ref is not in the given map.
-}
-- TODO: It should be an invariant that we have initialised the ref before
-- calling getMapRef, so that this function never fails.
{-# INLINE getMapRef #-}
getMapRef :: RefMap Word64 v -> RT (ERef v)
getMapRef wsMap = unsafeIOToRT 
  ((Map.!) <$> readIORef wsMap <*> myThreadIDKey)

{-| 
Atomically set the `Ref` associated with this thread with value @v@.
-}
setMapRef :: RefMap Word64 v -> v -> RT ()
setMapRef wsMap ws = newRef ws $ \ref -> unsafeIOToRT $ do
  tid <- myThreadIDKey
  atomicModifyIORef' 
    wsMap 
    (\mp -> (Map.insert tid (ERef ref) mp , ()))


{-|
Replace the whitespace parser for this thread.

This has no race conditions as:

* the `Ref` entry in the map stays the same, instead the value of the `Ref` is modified, 
* threads only access their own associated `Ref`.

This parser consumes no input and always succeeds.
-}
{-# INLINE setWs #-}
setWs :: RefMap Word64 (Parsec ()) -- ^ `IORef` to the map of whitespace parsers
      -> Parsec () -- ^ @ws@, the new whitespace parser for this thread
      -> Parsec () -- ^ 
setWs wsMap ws = Parsec $ \st good _ ->
  do  ERef ref <- getMapRef wsMap
      writeRef ref ws
      good () st

{-|
Initialise the `Ref` to the whitespace parser for this thread, with the given parser.

This has no race conditions as it uses `setMapRef`, which atomically updates the `RefMap`.

This parser consumes no input and always succeeds.
-}
{-# INLINE initWs #-}
initWs :: RefMap Word64 (Parsec ())
       -> WkThreadMap
       -> Parsec ()
       -> Parsec ()
initWs wsMap ts ws = Parsec $ \st good _ -> do
  updateThreadMaps ts wsMap ws
  good () st

{-|
Run a parser and, if it fails __without consuming input__, undo its modifications to the 
current thread's whitespace parser.

This parser consumes input only if @p@ does also; 
it fails if and only if @p@ fails __having consumed input__.
-}
{-# INLINE rollbackWs #-}
rollbackWs  :: RefMap Word64 (Parsec ())
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
setWsDuring :: RefMap Word64 (Parsec ())
            -> Parsec () -- ^ @ws@, the new temporary whitespace parser
            -> Parsec a  -- ^ @p@, the parser to run with the modified whitespace parser
            -> Parsec a  -- ^ a parser which runs @p@ with the new whitespace parser, 
                        -- and resets the old whitespace parser if @p@ succeeds.
setWsDuring wsMap ws p = do
  oldWs <- getWs wsMap
  setWs wsMap ws
  p <* setWs wsMap oldWs

mkSpace :: Desc.SpaceDesc -> ErrorConfig -> Space
mkSpace desc@Desc.SpaceDesc{..} !errConfig = Space {..}
  where
  -- don't think we can trust doing initialisation here, it'll happen in some random order
  -- This is the global ref which holds the whitespace implementation for parsers where 
  -- 'Text.Gigaparsec.Token.Descriptions.whiteSpaceIsContextDependent' is true.
  {-
  A per-lexer ref to the mapping from thread id hashes to their corresponding whitespace parsers.

  Implemented in terms of `unsafePerformIO` -- use with caution!
  -}
  {-# NOINLINE wsImplMap #-}
  wsImplMap :: RefMap Word64 (Parsec ())
  !wsImplMap = unsafePerformIO (newIORef Map.empty)

  {-
  A per-lexer `IORef` to the mapping from thread-id hashes to `Weak` thread ids.

  Implemented in terms of `unsafePerformIO` -- use with caution!
  -}
  {-# NOINLINE threadTIDMap #-}
  threadTIDMap :: WkThreadMap
  !threadTIDMap = unsafePerformIO (newIORef [])

  comment = commentParser desc -- do not make this strict
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
    | whitespaceIsContextDependent = initWs wsImplMap threadTIDMap configuredWhitespace
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
