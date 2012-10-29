{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.Records.Rec where

import Data.Records.Field
import GHC.TypeLits

data k :=: x = k :=: x deriving Show

-- A record is parameterized by a list of fields.
data Rec :: [*] -> * where
  Record :: Rec '[]
  (:-) :: (f ~ (sy ::: t)) => Rec fs -> f :=: t -> Rec (f ': fs)
infixl 8 :-

-- Type level append
type family (as :: [*]) :++ (bs :: [*]) :: [*]
type instance '[]       :++ bs = bs
type instance (a ': as) :++ bs = a ': (as :++ bs)

-- Creates singleton Record
(=:) :: sy ::: t -> t -> Rec '[sy ::: t]
a =: b = Record :- a :=: b

-- Appends Records
(<:>) :: Rec as -> Rec bs -> Rec (bs :++ as)
as <:> Record = as
as <:> (bs :- b) = (as <:> bs) :- b
infixl 8 <:>

instance Show (Rec '[]) where
  show Record = "{}"
instance (SingI sy, Show t) => Show (Rec ((sy ::: t) ': '[])) where
  show (Record :- kx) = "Record: " ++ show kx
instance (SingI sy, Show t, Show (Rec fs)) => Show (Rec ((sy ::: t) ': fs)) where
  show (xs :- kx) = show xs ++ ", " ++ show kx

