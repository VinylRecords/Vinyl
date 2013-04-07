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
import           Data.Monoid
import           Data.Vinyl.Field
import           GHC.TypeLits

-- A record is parameterized by a list of fields and a functor
-- to be applied to each of those fields.
data Rec :: [*] -> (* -> *) -> * where
  RNil :: Rec '[] f
  (:&) :: (r ~ (sy ::: t)) => f t -> Rec rs f -> Rec (r ': rs) f
infixr :&

-- Fixes a polymorphic record into the identity functor.
fixRecord :: (forall f. Applicative f => Rec rs f) -> PlainRec rs
fixRecord xs = xs

-- Fields of plain records are in the Identity functor.
type PlainRec rs = Rec rs Identity

-- Appends records
(<+>) :: Rec as f -> Rec bs f -> Rec (as ++ bs) f
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixl 8 <+>

-- Shorthand for a record with a single field
(=:) :: Applicative f => sy ::: t -> t -> Rec '[sy ::: t] f
a =: b = pure b :& RNil

-- Type level list append
type family (as :: [*]) ++ (bs :: [*]) :: [*]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)

-- Type Class Instances
instance Show (Rec '[] f) where
  show RNil = "{}"

instance (SingI sy, Show (g t), Show (Rec fs g)) => Show (Rec ((sy ::: t) ': fs) g) where
  show (x :& xs) = show (Field :: sy ::: t) ++ " :=: " ++ show x ++ ", " ++ show xs where

instance Eq (Rec '[] f) where
  _ == _ = True
instance (Eq (g t), Eq (Rec fs g)) => Eq (Rec ((s ::: t) ': fs) g) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Apply (~>) (Rec rs) where
  RNil <<*>> RNil = RNil
  (f :& fs) <<*>> (x :& xs) = runNT f x :& (fs <<*>> xs)

instance Run (Rec rs) where
  run RNil      = pure RNil
  run (x :& xs) = (:&) <$> (pure <$> x) <*> run xs

instance Show a => Show (Identity a) where
  show (Identity x) = show x


{-
ex :: Rec Name Identity
ex = Identity "jon"
  :& Identity "sterling"
  :& Identity 20
  :& RNil

verifyFirst = NT $ \str -> Failure ["asdfadS"]
verifyLast = NT $ \str -> Failure ["uhoh"]
verifyAge = NT $ \_ -> Failure ["uhoh"]

ex2 :: Rec Name (Identity ~> Result [String])
ex2 = verifyFirst
    :& verifyLast
    :& verifyAge
    :& RNil

ex3 = ex2 <<*>> ex
ex4 = run ex3
-}
