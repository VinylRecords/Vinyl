{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Vinyl.Core where

import Data.Vinyl.TyFun
import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))

-- | A record is parameterized by a universe @u@, list of rows @rs@, a large
-- elimination @el@, and a type constructor @f@ to be applied to the
-- interpretation @el r@ of each of those @r@.
data Rec :: (TyFun u * -> *) -> [u] -> * where
  RNil :: Rec el '[]
  (:&) :: !(el $ r) -> !(Rec el rs) -> Rec el (r ': rs)
infixr :&

-- | Shorthand for a record with a single field.
(=:) :: sing k -> el $ k -> Rec el '[ k ]
_ =: x = x :& RNil

-- | Shorthand for a record with a single field. This is useful for
-- @Applicative@ or @Monad@ic intialization of records as in the idiom:
--
-- > dist $ myField <-: someIO <+> yourField <-: otherIO
(<-:) :: sing r -> f (el $ r) -> Rec (f :. el) '[r]
_ <-: x = x :& RNil
infixr 6 <-:

-- | Records constructed using the above combinators will often be polymorphic
-- in their interpreter @el@. To avoid providing a type annotation, one can
-- provide their interpreters with a singleton tag and pass that in.
withUniverse :: (forall x. el x) -> Rec el rs -> Rec el rs
withUniverse _ x = x
{-# INLINE withUniverse #-}

instance Monoid (Rec el '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (el $ r), Monoid (Rec el rs)) => Monoid (Rec el (r ': rs)) where
  mempty = mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = mappend x y :& (xs `mappend` ys)

instance Eq (Rec el '[]) where
  _ == _ = True
instance (Eq (el $ r), Eq (Rec el rs)) => Eq (Rec el (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Storable (Rec el '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (el $ r), Storable (Rec el rs)) => Storable (Rec el (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: el $ r) + sizeOf (undefined :: Rec el rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined :: el $ r)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: el $ r))
                return $ x :& xs
  {-# INLINABLE peek #-}
  poke ptr (!x :& xs) = poke (castPtr ptr) x >>
                                 poke (ptr `plusPtr` sizeOf (undefined :: el $ r)) xs
  {-# INLINEABLE poke #-}

