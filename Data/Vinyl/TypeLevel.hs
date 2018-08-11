{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.TypeLevel where

import GHC.Exts

-- | A mere approximation of the natural numbers. And their image as lifted by
-- @-XDataKinds@ corresponds to the actual natural numbers.
data Nat = Z | S !Nat

-- | Produce a runtime 'Int' value corresponding to a 'Nat' type.
class NatToInt (n :: Nat) where
  natToInt :: Int

instance NatToInt 'Z where
  natToInt = 0
  {-# INLINE natToInt #-}

instance NatToInt n => NatToInt ('S n) where
  natToInt = 1 + natToInt @n
  {-# INLINE natToInt #-}

-- | Reify a list of type-level natural number indices as runtime
-- 'Int's relying on instances of 'NatToInt'.
class IndexWitnesses (is :: [Nat]) where
  indexWitnesses :: [Int]

instance IndexWitnesses '[] where
  indexWitnesses = []
  {-# INLINE indexWitnesses #-}

instance (IndexWitnesses is, NatToInt i) => IndexWitnesses (i ': is) where
  indexWitnesses = natToInt @i : indexWitnesses @is
  {-# INLINE indexWitnesses #-}

-- | Project the first component of a type-level tuple.
type family Fst (a :: (k1,k2)) where Fst '(x,y) = x

-- | Project the second component of a type-level tuple.
type family Snd (a :: (k1,k2)) where Snd '(x,y) = y

type family RLength xs where
  RLength '[] = 'Z
  RLength (x ': xs) = 'S (RLength xs)

-- | A partial relation that gives the index of a value in a list.
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

-- | Remove the first occurence of a type from a type-level list.
type family RDelete r rs where
  RDelete r (r ': rs) = rs
  RDelete r (s ': rs) = s ': RDelete r rs

-- | A constraint-former which applies to every field in a record.
type family RecAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  RecAll f '[] c = ()
  RecAll f (r ': rs) c = (c (f r), RecAll f rs c)

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | Constraint that all types in a type-level list satisfy a
-- constraint.
type family AllConstrained (c :: u -> Constraint) (ts :: [u]) :: Constraint where
  AllConstrained c '[] = ()
  AllConstrained c (t ': ts) = (c t, AllConstrained c ts)

-- | Constraint that each Constraint in a type-level list is satisfied
-- by a particular type.
class AllSatisfied cs t where
instance AllSatisfied '[] t where
instance (c t, AllSatisfied cs t) => AllSatisfied (c ': cs) t where

-- | Constraint that all types in a type-level list satisfy each
-- constraint from a list of constraints.
--
-- @AllAllSat cs ts@ should be equivalent to @AllConstrained
-- (AllSatisfied cs) ts@ if partial application of type families were
-- legal.
type family AllAllSat cs ts :: Constraint where
  AllAllSat cs '[] = ()
  AllAllSat cs (t ': ts) = (AllSatisfied cs t, AllAllSat cs ts)
