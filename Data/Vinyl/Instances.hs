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
import Data.Singletons
import Data.Monoid
import Control.Applicative
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Idiom.Identity

instance Show (Rec '[] f) where
  show RNil = "{}"
instance forall g (r::k) fs. (
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    KnownSymbol sy,
#else
    SingI r,
#endif
    Show (g (El r)), SingKind ('KProxy :: KProxy k),
    Show (DemoteRep ('KProxy :: KProxy k)), Show (Rec fs g)) => Show (Rec (r ': fs) g) where
  show (x :& xs) = show (fromSing (sing :: Sing r)) ++ " :=: " ++ show x ++ ", " ++ show xs

instance Eq (Rec '[] f) where
  _ == _ = True
instance (Eq (f (El r)), Eq (Rec rs f)) => Eq (Rec (r ': rs) f) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Monoid (Rec '[] f) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (El r), Monoid (Rec rs f), Applicative f) => Monoid (Rec (r ': rs) f) where
  mempty = pure mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = liftA2 mappend x y :& (xs `mappend` ys)

instance Storable (PlainRec '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (El r), Storable (PlainRec rs)) => Storable (PlainRec (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: El r) + sizeOf (undefined :: PlainRec rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined :: El r)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: El r))
                return $ Identity x :& xs
  {-# INLINABLE peek #-}
  poke ptr (Identity !x :& xs) = poke (castPtr ptr) x >>
                                 poke (ptr `plusPtr` sizeOf (undefined :: El r)) xs
  {-# INLINEABLE poke #-}
