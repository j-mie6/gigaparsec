{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
This module defines a strict mapping from `Word64`s of raw `ThreadId`s to @`Weak` `ThreadId`@.

For use in managing the white-space parsers on a per-thread basis 
(See `Text.Gigaparsec.Internal.Token.Space.mkSpace`).
-}
module Text.Gigaparsec.Internal.Token.Space.WkThreadMap (
  -- * `WkThreadMap` API
  WkThreadMap(),
  empty,
  insert,
  -- * Partition Operations
  partitionThreadsThenAdd,
  partitionThreads
  ) where

import Prelude hiding (concat)
import Data.Word (Word64)
import Control.Concurrent (ThreadId)
import System.Mem.Weak (Weak, deRefWeak)
import GHC.IO (unsafePerformIO)
import Data.Maybe (isJust)


-- | A strict pair of `Word64` and @`Weak` `ThreadId`@s.
-- The `Word64` represents the raw `ThreadId` as per `fromThreadId`.
-- This should be the same `ThreadId` as the weak pointer.
type StrictWTWord :: *
data StrictWTWord = 
  WTW { 
      -- | The raw `ThreadId` of the thread, as given by `fromThreadId`
      unTID      :: {-# UNPACK #-} !Word64,
      -- | The weak pointer to the thread with the given id.
      unWkThread :: {-# UNPACK #-} !(Weak ThreadId)
    }

{-| A strict map of `Word64` to its associated @`Weak` `ThreadId`@.
The `Word64` represents the raw `ThreadId` as per `fromThreadId`.
This should be the same `ThreadId` as the weak pointer.

Strict in every component: the `Word64`, the @`Weak` `ThreadId`@, and the spine of the map.
-}
-- Best not to expose the constructors if we decide to change how the map is implemented.
type WkThreadMap :: *
data WkThreadMap =
    -- Empty WkThreadMap
    Nil
  | WkThread
      -- | The raw `ThreadId` value.
      {-# UNPACK #-}
      !Word64
      -- | Weak pointer to the thread with the given id.
      {-# UNPACK #-} !(Weak ThreadId)
      -- | The rest of the map.
      !WkThreadMap

-- | Add a thread to the `WkThreadMap`.
-- Strict in all arguments.
{-# INLINE insert #-}
insert :: Word64 -> Weak ThreadId -> WkThreadMap -> WkThreadMap
insert !w !p !xs = WkThread w p xs

{-# INLINE empty #-}
empty :: WkThreadMap
empty = Nil

{-| Combine two thread maps.
 
Does not check for duplicate keys.
-}
-- TODO: Unused: remove?
concat :: WkThreadMap -> WkThreadMap -> WkThreadMap
concat Nil xs = xs
concat (WkThread x t xs) ys = WkThread x t (concat xs ys)

-- | Behaves as `partitionThreads`, but then adds the new thread onto the resulting `alive` map.
partitionThreadsThenAdd :: Word64 -> Weak ThreadId -> WkThreadMap -> (WkThreadMap, [Word64])
partitionThreadsThenAdd x t mp =
  let (!alive, !dead) = partitionThreads mp
  in  (WkThread x t alive, dead)

-- | Partition the weak-thread map by those who are still alive (left), and those
-- who are dead (right).
-- we just return the unique `Word64` for the dead threads.
partitionThreads :: WkThreadMap -> (WkThreadMap, [Word64])
partitionThreads mp = partition mp isAlive
  where
  -- Determine if the thread is still alive.
  -- WARNING: defined with `unsafePerformIO`, use with caution.
  {-# NOINLINE isAlive #-}
  isAlive :: Weak ThreadId -> Bool
  isAlive = unsafePerformIO . (isJust <$>) . deRefWeak

  -- Similar to `Data.List.partition`.
  -- Split the `WkThreadMap` into those that satisfy the predicate (left),
  -- and the raw ThreadIds of those that do not (right)
  partition ::  WkThreadMap -> (Weak ThreadId -> Bool) -> (WkThreadMap, [Word64])
  partition Nil _ = (Nil, [])
  partition (WkThread x t xs) f =
    let (!alive, !dead) = partition xs f
    in  if f t
      then (WkThread x t alive, dead)
      else (alive, x: dead)


