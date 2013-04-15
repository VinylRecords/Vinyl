{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Vinyl.Rec
  ( Rec(..)
  , PlainRec
  , (=:)
  , (<+>)
  , fixRecord
  ) where

import           Data.Vinyl.Classes
import           Control.Applicative
import           Control.Monad.Identity
import           Data.Vinyl.Field
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable(..))
import           GHC.TypeLits

-- | A record is parameterized by a list of fields and a functor
-- to be applied to each of those fields.
data Rec :: [*] -> (* -> *) -> * where
  RNil :: Rec '[] f
  (:&) :: f t -> Rec rs f -> Rec ((sy ::: t) ': rs) f
infixr :&

-- | Fixes a polymorphic record into the 'Identity' functor.
fixRecord :: (forall f. Applicative f => Rec rs f) -> PlainRec rs
fixRecord xs = xs

-- | Fields of plain records are in the 'Identity' functor.
type PlainRec rs = Rec rs Identity

-- | Append for records.
(<+>) :: Rec as f -> Rec bs f -> Rec (as ++ bs) f
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixl 8  <+>

-- | Shorthand for a record with a single field. Lifts the field's
-- value into the chosen functor automatically.
(=:) :: Applicative f => sy ::: t -> t -> Rec '[sy ::: t] f
a =: b = pure b :& RNil

-- | Append for type-level lists.
type family (as :: [*]) ++ (bs :: [*]) :: [*]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)


instance Show (Rec '[] f) where
  show RNil = "{}"
instance (SingI sy, Show (g t), Show (Rec fs g)) => Show (Rec ((sy ::: t) ': fs) g) where
  show (x :& xs) = show (Field :: sy ::: t) ++ " :=: " ++ show x ++ ", " ++ show xs


instance Eq (Rec '[] f) where
  _ == _ = True
instance (Eq (g t), Eq (Rec fs g)) => Eq (Rec ((s ::: t) ': fs) g) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

-- | Records can be applied to each other.
instance Apply (~>) (Rec rs) where
  RNil <<*>> RNil = RNil
  (f :& fs) <<*>> (x :& xs) = runNT f x :& (fs <<*>> xs)

-- | Records may be 'run' to accumulate the effects of their fields.
instance Run (Rec rs) where
  run RNil      = pure RNil
  run (x :& xs) = (:&) <$> (pure <$> x) <*> run xs

-- | We provide a 'Show' instance for 'Identity'.
instance Show a => Show (Identity a) where
  show (Identity x) = show x

instance Storable (Rec '[] Identity) where
  sizeOf _ = 0
  alignment _ = 0
  peek _ = return RNil
  poke _ RNil = return ()

instance (Storable t, Storable (PlainRec rs))
  => Storable (PlainRec ((sy:::t) ': rs)) where
  sizeOf _ = sizeOf (undefined::t) + sizeOf (undefined::PlainRec rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined::t)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined::t))
                return $ Identity x :& xs
  {-# INLINABLE peek #-}
  poke ptr (Identity !x :& xs) = poke (castPtr ptr) x >>
                                 poke (ptr `plusPtr` sizeOf (undefined::t)) xs
  {-# INLINEABLE poke #-}
