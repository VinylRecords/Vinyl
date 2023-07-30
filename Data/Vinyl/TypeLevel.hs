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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                   #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE UndecidableInstances  #-}
#endif

module Data.Vinyl.TypeLevel where

import Data.Coerce
import Data.Kind

{- |
A mere approximation of the natural numbers. And their image as lifted by
@-XDataKinds@ corresponds to the actual natural numbers.

The following is an example for a term level number of type Nat:

>>> number = S (S Z)
>>> :t number
number :: Nat

But it is intended to be used at the type level:

>>> import Data.Proxy (Proxy(Proxy))
>>> Proxy :: Proxy (S (S Z))
Proxy

Often it is used with type applications:

>>> Proxy @(S (S Z))
Proxy
-}
data Nat = Z | S !Nat

{- |
Produce a runtime 'Int' value corresponding to a 'Nat' type.
This is intended to be used with TypeApplications:

>>> natToInt @('S ('S 'Z))
2
-}
class NatToInt (n :: Nat) where
  natToInt :: Int

instance NatToInt 'Z where
  natToInt = 0
  {-# INLINE natToInt #-}

instance NatToInt n => NatToInt ('S n) where
  natToInt = 1 + natToInt @n
  {-# INLINE natToInt #-}

{- |
Reify a list of type-level natural number indices as runtime
'Int's relying on instances of 'NatToInt'.

>>> indexWitnesses @'[S (S Z), Z, S (S (S Z))]
[2,0,3]
-}
class IndexWitnesses (is :: [Nat]) where
  indexWitnesses :: [Int]

instance IndexWitnesses '[] where
  indexWitnesses = []
  {-# INLINE indexWitnesses #-}

instance (IndexWitnesses is, NatToInt i) => IndexWitnesses (i ': is) where
  indexWitnesses = natToInt @i : indexWitnesses @is
  {-# INLINE indexWitnesses #-}

{- |
Project the first component of a type-level tuple.

>>> :k! Fst '("age", Int)
Fst '("age", Int) :: GHC.Types.Symbol
= "age"
-}
type family Fst (a :: (k1,k2)) where Fst '(x,y) = x

{- |
Project the second component of a type-level tuple.

>>> :k! Snd '("age", Int)
Snd '("age", Int) :: *
= Int
-}
type family Snd (a :: (k1,k2)) where Snd '(x,y) = y

{- |
Compute the length of a type level list.

>>> :k! RLength '[ '("age", Int), '("name", String)]
RLength '[ '("age", Int), '("name", String)] :: Nat
= 'S ('S 'Z)
-}
type family RLength xs where
  RLength '[] = 'Z
  RLength (x ': xs) = 'S (RLength xs)

{- |
Compute the index of a value in a list.

>>> :k! RIndex '("age", Int) '[ '("age", Int), '("name", String)]
RIndex '("age", Int) '[ '("age", Int), '("name", String)] :: Nat
= 'Z
-}
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)

{- |
Compute the indices of a sublist in a larger list.

>>> :k! RImage '[Int, String] '[Int, Double, String]
RImage '[Int, String] '[Int, Double, String] :: [Nat]
= '[ 'Z, 'S ('S 'Z)]
-}
type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

{- |
Remove the first occurrence of a type from a type-level list.

>>> :k! RDelete Int '[Int, Double, String]
RDelete Int '[Int, Double, String] :: [*]
= '[Double, String]
-}
type family RDelete r rs where
  RDelete r (r ': rs) = rs
  RDelete r (s ': rs) = s ': RDelete r rs

-- | A constraint-former which applies to every field in a record.
type family RecAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  RecAll f '[] c = ()
  RecAll f (r ': rs) c = (c (f r), RecAll f rs c)

infixr 5 ++

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

-- | Apply a type constructor to a record index. Record indexes are
-- either 'Type' or @('Symbol', 'Type')@. In the latter case, the type
-- constructor is applied to the second component of the tuple.
type family ApplyToField (t :: Type -> Type) (a :: k1) = (r :: k1) | r -> t a where
  ApplyToField t '(s,x) = '(s, t x)
  ApplyToField t x = t x

-- | Apply a type constructor to each element of a type level list
-- using 'ApplyOn'.
type family MapTyCon t xs = r | r -> xs where
  MapTyCon t '[] = '[]
  MapTyCon t (x ': xs) = ApplyToField t x ': MapTyCon t xs

-- | This class is used for `consMatchCoercion` with older versions
-- of GHC.
class Coercible (f x) (g x) => Similar f g (x :: k)
instance Coercible (f x) (g x) => Similar f g (x :: k)
