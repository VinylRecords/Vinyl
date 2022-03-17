{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

-- | Helper functions for SmallArray#
--
-- This module exposes _unsafe_ functions to work with SmallArrays.  That means
-- that specifically neither index bounds nor element types are checked So this
-- functionality should only be used in a context that enforces them by some
-- other means, e.g., ARec's type index

module Data.Vinyl.ARec.Internal.SmallArray where

import GHC.Prim
import GHC.Types
import Unsafe.Coerce
import GHC.ST

data SmallArray = SmallArray !(SmallArray# Any)
data SmallMutableArray s = SmallMutableArray !(SmallMutableArray# s Any)

indexSmallArray :: SmallArray -> Int -> a
indexSmallArray (SmallArray arr) (I# ix) =
  case indexSmallArray# arr ix of
    (# v #) -> unsafeCoerce v
{-# INLINE indexSmallArray #-}

withNewSmallArray :: Int -> (SmallMutableArray s -> ST s ()) -> ST s SmallArray
withNewSmallArray (I# len#) f =
  ST $ \s0 ->  case newSmallArray# len# (error "withNewSmallArray exploded") s0 of
       (# s1, mArr #) ->
         case f (SmallMutableArray mArr) of
           ST st -> case st s1 of
             (# s2, () #) -> case unsafeFreezeSmallArray# mArr s2 of
               (# s3, ar #) -> (# s3, SmallArray ar #)
{-# INLINE withNewSmallArray #-}

writeSmallArray :: SmallMutableArray s -> Int -> a -> ST s ()
writeSmallArray (SmallMutableArray mArr) (I# n#) x = ST $ \s ->
  case writeSmallArray# mArr n# (unsafeCoerce x) s of
    s' -> (# s', () #)
{-# INLINE writeSmallArray #-}

withThawedSmallArray :: SmallArray
               -> (SmallMutableArray s -> ST s ())
               -> ST s SmallArray
withThawedSmallArray (SmallArray arr) f = ST $ \s0 ->
  let !(I# z#) = 0
  in case thawSmallArray# arr z# (sizeofSmallArray# arr) s0 of
       (# s1, mArr #) ->
         case f (SmallMutableArray mArr) of
           ST st -> case st s1 of
             (# s2, () #) -> case unsafeFreezeSmallArray# mArr s2 of
               (# s3, ar #) -> (# s3, SmallArray ar #)
{-# INLINE withThawedSmallArray #-}
