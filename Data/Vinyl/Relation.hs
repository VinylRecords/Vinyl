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

module Data.Vinyl.Relation
  ( (<:)(..)
  , (:~:)
  , (~=)
  , RecAll
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Lens
import Data.Vinyl.Witnesses
import Data.Vinyl.TyFun
import GHC.Prim (Constraint)

-- | One record is a subtype of another if the fields of the latter are a
-- subset of the fields of the former.
class (xs :: [k]) <: (ys :: [k]) where
  cast :: Rec el f xs -> Rec el f ys

instance xs <: '[] where
  cast _ = RNil

instance (IElem y xs, xs <: ys) => xs <: (y ': ys) where
  cast xs = ith (implicitly :: Elem y xs) xs :& cast xs
    where
      ith :: Elem r rs -> Rec el f rs -> f (el $ r)
      ith Here (a :& _) = a
      ith (There p) (_ :& as) = ith p as

-- | If two records types are subtypes of each other, that means that they
-- differ only in order of fields.
type r1 :~: r2 = (r1 <: r2, r2 <: r1)

-- | Term-level record congruence.
(~=) :: (Eq (Rec el f xs), xs :~: ys) => Rec el f xs -> Rec el f ys -> Bool
x ~= y = x == (cast y)

type family RecAll (el :: TyFun k l -> *) (f :: * -> *) (rs :: [k]) (c :: * -> Constraint) :: Constraint
type instance RecAll el f '[] c = ()
type instance RecAll el f (r ': rs) c = (c (f (el $ r)), RecAll el f rs c)

