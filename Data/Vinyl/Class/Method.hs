{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns          #-}

module Data.Vinyl.Class.Method 
  ( -- * Eq Functions
    recEq
    -- * Ord Functions
  , recCompare
    -- * Monoid Functions
  , recMempty
  , recMappend
  , recMconcat
    -- * Num Functions
  , recAdd
  , recSubtract
  , recMultiply
  , recAbs
  , recSignum
  , recNegate
    -- * Bounded Functions
  , recMinBound
  , recMaxBound
  ) where

import Data.Vinyl.Core
import Data.Vinyl.TypeLevel
import Data.Monoid

recEq :: RecAll f rs Eq => Rec f rs -> Rec f rs -> Bool
recEq RNil RNil = True
recEq (a :& as) (b :& bs) = a == b && recEq as bs

recCompare :: RecAll f rs Ord => Rec f rs -> Rec f rs -> Ordering
recCompare RNil RNil = EQ
recCompare (a :& as) (b :& bs) = compare a b <> recCompare as bs

-- | This function differs from the original 'mempty' in that 
--   it takes an argument. In some cases, you will already
--   have a record of the type you are interested in, and 
--   that can be passed an the argument. In other situations
--   where this is not the case, you may need the
--   interpretation function of the argument record to be 
--   @Const ()@ or @Proxy@ so the you can generate the
--   argument with 'rpure'.
recMempty :: RecAll f rs Monoid => Rec proxy rs -> Rec f rs
recMempty RNil = RNil
recMempty (_ :& rs) = mempty :& recMempty rs

recMappend :: RecAll f rs Monoid => Rec f rs -> Rec f rs -> Rec f rs
recMappend RNil RNil = RNil
recMappend (a :& as) (b :& bs) = mappend a b :& recMappend as bs

-- | This function differs from the original 'mconcat'.
--   See 'recMempty'.
recMconcat :: RecAll f rs Monoid => Rec proxy rs -> [Rec f rs] -> Rec f rs
recMconcat p [] = recMempty p
recMconcat p (rec : recs) = recMappend rec (recMconcat p recs)

recAdd :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recAdd RNil RNil = RNil
recAdd (a :& as) (b :& bs) = (a + b) :& recAdd as bs

recSubtract :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recSubtract RNil RNil = RNil
recSubtract (a :& as) (b :& bs) = (a - b) :& recSubtract as bs

recMultiply :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recMultiply RNil RNil = RNil
recMultiply (a :& as) (b :& bs) = (a * b) :& recSubtract as bs

recAbs :: RecAll f rs Num => Rec f rs -> Rec f rs
recAbs RNil = RNil
recAbs (a :& as) = abs a :& recAbs as 

recSignum :: RecAll f rs Num => Rec f rs -> Rec f rs
recSignum RNil = RNil
recSignum (a :& as) = signum a :& recAbs as 

recNegate :: RecAll f rs Num => Rec f rs -> Rec f rs
recNegate RNil = RNil
recNegate (a :& as) = negate a :& recAbs as 

-- | This function differs from the original 'minBound'.
--   See 'recMempty'.
recMinBound :: RecAll f rs Bounded => Rec proxy rs -> Rec f rs
recMinBound RNil = RNil
recMinBound (_ :& rs) = minBound :& recMinBound rs

-- | This function differs from the original 'maxBound'.
--   See 'recMempty'.
recMaxBound :: RecAll f rs Bounded => Rec proxy rs -> Rec f rs
recMaxBound RNil = RNil
recMaxBound (_ :& rs) = maxBound :& recMaxBound rs

