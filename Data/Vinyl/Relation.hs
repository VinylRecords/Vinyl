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
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vinyl.Relation
  ( (<:)(..)
  , (:~:)
  , (~=)
  , rIso
  ) where

import           Data.Vinyl.Field
import           Data.Vinyl.Lens
import           Data.Vinyl.Rec
import           Data.Vinyl.Witnesses

import           GHC.Prim             (Constraint)

-- | A subtyping relation.
class (IsSubtype r1 r2) => r1 <: r2 where
  cast :: r1 -> r2

-- | One record is a subtype of another if the fields of the latter are a
-- subset of the fields of the former.
type family IsSubtype r1 r2 :: Constraint
type instance IsSubtype (Rec ss f) (Rec ts f) = ISubset ts ss

-- | If two records types are subtypes of each other, that means that they
-- differ only in order of fields.
type r1 :~: r2 = (r1 <: r2, r2 <: r1)

-- | Term-level record congruence.
(~=) :: (Eq a, a :~: b) => a -> b -> Bool
x ~= y = x == (cast y)

instance Rec xs f <: Rec '[] f where
  cast _ = RNil

instance (y ~ (sy ::: t), IElem y xs, Rec xs f <: Rec ys f) => Rec xs f <: Rec (y ': ys) f where
  cast r = (r^.(rLens' field)) :& cast r
    where field = lookupField (implicitly :: Elem y xs) r

lookupField :: Elem x xs -> Rec xs f -> x
lookupField Here      (_ :& _)  = Field
lookupField (There p) (_ :& xs) = lookupField p xs

rIso :: (r1 :~: r2) => Iso' r1 r2
rIso = iso cast cast

