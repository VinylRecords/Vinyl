{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.TypeLevel where

import GHC.Exts

-- | An approximation of the natural numbers. And their image as lifted by
-- -XDataKinds corresponds to the actual natural numbers.
data Nat = Z | S !Nat

-- | A partial relation that gives the index of a value in a list.
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = Z
  RIndex r (s ': rs) = S (RIndex r rs)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

-- | A constraint-former which applies to every field in a record.
type family RecAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  RecAll f '[] c = ()
  RecAll f (r ': rs) c = (c (f r), RecAll f rs c)

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

