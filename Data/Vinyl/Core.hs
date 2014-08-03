{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Core where

import Data.Vinyl.TyFun
import Control.Applicative
import Data.Monoid
import Data.Vinyl.Idiom.Identity
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))

-- | A record is parameterized by a universe @u@, list of rows @rs@, a large
-- elimination @el@, and a type constructor @f@ to be applied to the
-- interpretation @el r@ of each of those @r@.
data Rec (el :: TyFun u * -> *) (f :: * -> *) (rrs :: [u]) where
  RNil :: Rec el f '[]
  (:&) :: !(f (el $ r)) -> !(Rec el f rs) -> Rec el f (r ': rs)
infixr :&

type x ∈ xs = RElem x xs (RIndex x xs)

data Nat = Z | S Nat
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = Z
  RIndex r (s ': rs) = S (RIndex r rs)

class i ~ RIndex r rs => RElem (r :: k) (rs :: [k]) (i :: Nat) where
  rlens' :: Functor g => sing r -> (f (el $ r) -> g (f (el $ r))) -> Rec el f rs -> g (Rec el f rs)

  rlens :: Functor g => sing r -> (el $ r -> g (el $ r)) -> Rec el Identity rs -> g (Rec el Identity rs)
  rlens r = rlens' r . (\sa sbt afb s -> sbt s <$> afb (sa s)) runIdentity (const Identity)
  {-# INLINE rlens #-}

instance RElem r (r ': rs) Z where
  rlens' _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rlens' #-}

instance (RIndex r (s ': rs) ~ S i, RElem r rs i) => RElem r (s ': rs) (S i) where
  rlens' p f (x :& xs) = fmap (x :&) (rlens' p f xs)
  {-# INLINE rlens' #-}

-- | Shorthand for a record with a single field. Lifts the field's
-- value into the chosen functor automatically.
(=:) :: Applicative f => sing k -> el $ k -> Rec el f '[ k ]
_ =: x = pure x :& RNil

-- | Shorthand for a record with a single field. This is useful for
-- @Applicative@ or @Monad@ic intialization of records as in the idiom:
--
-- > dist $ myField <-: someIO <+> yourField <-: otherIO
(<-:) :: sing r -> f (el $ r) -> Rec el f '[r]
_ <-: x = x :& RNil
infixr 6 <-:

-- | Records constructed using the above combinators will often be polymorphic
-- in their interpreter @el@. To avoid providing a type annotation, one can
-- provide their interpreters with a singleton tag and pass that in.
withUniverse :: (forall x. el x) -> Rec el f rs -> Rec el f rs
withUniverse _ x = x
{-# INLINE withUniverse #-}

instance Monoid (Rec el f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (el $ r), Monoid (Rec el f rs), Applicative f) => Monoid (Rec el f (r ': rs)) where
  mempty = pure mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = liftA2 mappend x y :& (xs `mappend` ys)

instance Eq (Rec el f '[]) where
  _ == _ = True
instance (Eq (f (el $ r)), Eq (Rec el f rs)) => Eq (Rec el f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Storable (Rec el Identity '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (el $ r), Storable (Rec el Identity rs)) => Storable (Rec el Identity (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: el $ r) + sizeOf (undefined :: Rec el Identity rs)
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

