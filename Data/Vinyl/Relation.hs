{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Vinyl.Relation
  ( (<:)(..)
  , (:~:)
  , (~=)
  , rIso
  ) where

import Data.Vinyl.Witnesses
import Data.Vinyl.Field
import Data.Vinyl.Rec
import Data.Vinyl.Lens

import GHC.Prim (Constraint)

-- A subtyping relation
class (IsSubtype r1 r2) => r1 <: r2 where
  cast :: r1 -> r2

-- On record is a subtype of another if the fields of the latter are a
-- subset of the fields of the former.
type family IsSubtype r1 r2 :: Constraint
type instance IsSubtype (Rec ss) (Rec ts) = ISubset ts ss

-- If two records types are subtypes of each other, that means that they
-- differ only in order of fields.
type r1 :~: r2 = (r1 <: r2, r2 <: r1)

-- Term-level record congruence
(~=) :: (Eq a, a :~: b) => a -> b -> Bool
x ~= y = x == (cast y)

instance Rec xs <: (Rec '[]) where
  cast _ = RNil

instance (y ~ (sy ::: t), IElem y xs, Rec xs <: Rec ys) => Rec xs <: Rec (y ': ys) where
  cast r = (field, rGet field r) :& cast r
    where field = lookupField implicitly r

lookupField :: Elem x xs -> Rec xs -> x
lookupField Here ((k,_) :& _) = k
lookupField (There p) (_ :& xs) = lookupField p xs

rIso :: (r1 :~: r2) => SimpleIso r1 r2
rIso = iso cast cast

