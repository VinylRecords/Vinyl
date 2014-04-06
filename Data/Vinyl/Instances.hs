{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Instances where

import Data.Vinyl.Core
import Data.Vinyl.Derived
import Data.Vinyl.TyFun

import Data.Singletons
import Data.Monoid
import Control.Applicative
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Idiom.Identity

instance Show (Rec el f '[]) where
  show RNil = "{}"
instance forall g (r::k) rs el. (
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    KnownSymbol sy,
#else
    SingI r,
#endif
    Show (g (el $ r)), SingKind ('KProxy :: KProxy k),
    Show (DemoteRep ('KProxy :: KProxy k)), Show (Rec el g rs)) => Show (Rec el g (r ': rs)) where
  show (x :& xs) = show (fromSing (sing :: Sing r)) ++ " :=: " ++ show x ++ ", " ++ show xs

instance Eq (Rec el f '[]) where
  _ == _ = True
instance (Eq (f (el $ r)), Eq (Rec el f rs)) => Eq (Rec el f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Monoid (Rec el f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (el $ r), Monoid (Rec el f rs), Applicative f) => Monoid (Rec el f (r ': rs)) where
  mempty = pure mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = liftA2 mappend x y :& (xs `mappend` ys)

instance Storable (PlainRec el '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (el $ r), Storable (PlainRec el rs)) => Storable (PlainRec el (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: el $ r) + sizeOf (undefined :: PlainRec el rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined :: el $ r)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: el $ r))
                return $ Identity x :& xs
  {-# INLINABLE peek #-}
  poke ptr (Identity !x :& xs) = poke (castPtr ptr) x >>
                                 poke (ptr `plusPtr` sizeOf (undefined :: el $ r)) xs
  {-# INLINEABLE poke #-}
