{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Vinyl.Rec
  ( Rec(..)
  , (=:)
  , (<+>)
  ) where

import Data.Vinyl.Field
import GHC.TypeLits

-- A record is parameterized by a list of fields.
data Rec :: [*] -> * where
  RNil :: Rec '[]
  (:&) :: (f ~ (sy ::: t)) => (f, t) -> Rec fs -> Rec (f ': fs)
infixr :&

-- Appends records
(<+>) :: Rec as -> Rec bs -> Rec (as ++ bs)
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixl 8 <+>

-- Shorthand for a record with a single field
(=:) :: sy ::: t -> t -> Rec '[sy ::: t]
a =: b = (a, b) :& RNil

-- Type level list append
type family (as :: [*]) ++ (bs :: [*]) :: [*]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)


instance Show (Rec '[]) where
  show RNil = "{}"
instance (SingI sy, Show t, Show (Rec fs)) => Show (Rec ((sy ::: t) ': fs)) where
  show ((k,x) :& xs) = show k ++ " :=: " ++ show x ++ ", " ++ show xs


instance Eq (Rec '[]) where
  _ == _ = True
instance (Eq t, Eq (Rec fs)) => Eq (Rec ((s ::: t) ': fs)) where
  ((_,x) :& xs) == ((_,y) :& ys) = (x == y) && (xs == ys)

