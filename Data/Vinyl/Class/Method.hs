{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns          #-}

module Data.Vinyl.Class.Method where

import Data.Vinyl.Core
import Data.Vinyl.TypeLevel
import Data.Monoid
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))

recEq :: RecAll f rs Eq => Rec f rs -> Rec f rs -> Bool
recEq RNil RNil = True
recEq (a :& as) (b :& bs) = a == b && recEq as bs

recCompare :: RecAll f rs Ord => Rec f rs -> Rec f rs -> Ordering
recCompare RNil RNil = EQ
recCompare (a :& as) (b :& bs) = compare a b <> recCompare as bs

recAdd :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recAdd RNil RNil = RNil
recAdd (a :& as) (b :& bs) = (a + b) :& recAdd as bs

recSubtract :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recSubtract RNil RNil = RNil
recSubtract (a :& as) (b :& bs) = (a - b) :& recSubtract as bs

recMulitply :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recMulitply RNil RNil = RNil
recMulitply (a :& as) (b :& bs) = (a * b) :& recSubtract as bs

recAbs :: RecAll f rs Num => Rec f rs -> Rec f rs
recAbs RNil = RNil
recAbs (a :& as) = abs a :& recAbs as 

recSignum :: RecAll f rs Num => Rec f rs -> Rec f rs
recSignum RNil = RNil
recSignum (a :& as) = signum a :& recAbs as 

recNegate :: RecAll f rs Num => Rec f rs -> Rec f rs
recNegate RNil = RNil
recNegate (a :& as) = negate a :& recAbs as 

recMappend :: RecAll f rs Monoid => Rec f rs -> Rec f rs -> Rec f rs
recMappend RNil RNil = RNil
recMappend (a :& as) (b :& bs) = mappend a b :& recMappend as bs

-- | This function differs from the original 'mempty' in that 
--   it takes an argument. In some cases, you will already
--   have a record of the type you are interested in, and 
--   that can be passed an the argument. In other situations
--   where this is not the case, you may need the
--   interpretation function of the argument record to be 
--   @Const ()@ or @Proxy@ so the you can generate the
--   argument with 'rpure'.
recMempty :: RecAll f rs Monoid => Rec g rs -> Rec f rs
recMempty RNil = RNil
recMempty (_ :& rs) = mempty :& recMempty rs

-- | This function differs from the original 'minBound'.
--   See 'recMempty'.
recMinBound :: RecAll f rs Bounded => Rec g rs -> Rec f rs
recMinBound RNil = RNil
recMinBound (_ :& rs) = minBound :& recMinBound rs

-- | This function differs from the original 'maxBound'.
--   See 'recMempty'.
recMaxBound :: RecAll f rs Bounded => Rec g rs -> Rec f rs
recMaxBound RNil = RNil
recMaxBound (_ :& rs) = maxBound :& recMaxBound rs

recSizeOf :: RecAll f rs Storable => Rec f rs -> Int
recSizeOf RNil = 0
recSizeOf (a :& as) = sizeOf a + recSizeOf as

-- Copied this from the Storable instance for Rec. I don't
-- know much about alignment, but it seems weird to 
-- just use the alignment of the record head.
-- Edit: as I think about it more, it seems like this 
-- makes sense, but I'll leave this note it until
-- someone confirms that this is correct.
recAlignment :: RecAll f rs Storable => Rec f rs -> Int
recAlignment RNil = 0
recAlignment (a :& _) = alignment a

-- | This function differs from the original 'peek'.
--   See 'recMempty'.
recPeek :: forall g f rs. RecAll f rs Storable => Rec g rs -> Ptr (Rec f rs) -> IO (Rec f rs)
recPeek RNil _ = return RNil
recPeek ((_ :: g r) :& rs) ptr = do
  !x <- peek (castPtr ptr)
  !xs <- recPeek rs (ptr `plusPtr` sizeOf (undefined :: f r))
  return $ x :& xs

recPoke :: RecAll f rs Storable => Ptr (Rec f rs) -> Rec f rs -> IO ()
recPoke _ RNil = return ()
recPoke ptr (r :& rs) = do
  poke (castPtr ptr) r
  recPoke (ptr `plusPtr` sizeOf r) rs

-- TODO: add RecAll versions of all of the other 
-- methods in Storable.

