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
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Lens
import Data.Vinyl.Witnesses

import GHC.Prim             (Constraint)
import Data.Singletons

-- | One record is a subtype of another if the fields of the latter are a
-- subset of the fields of the former.
class (xs :: [k]) <: (ys :: [k]) where
  cast :: Rec el f xs -> Rec el f ys

instance xs <: '[] where
  cast _ = RNil

instance (SingI y, IElem y xs, xs <: ys) => xs <: (y ': ys) where
  cast xs = rGet' (lookupField (implicitly :: Elem y xs) xs) xs :& cast xs
    where
      lookupField :: SingI r => Elem r rs -> Rec el f rs -> Sing r
      lookupField Here      (_ :& _)  = sing
      lookupField (There p) (_ :& xs) = lookupField p xs

-- | If two records types are subtypes of each other, that means that they
-- differ only in order of fields.
type r1 :~: r2 = (r1 <: r2, r2 <: r1)

-- | Term-level record congruence.
(~=) :: (Eq (Rec el f xs), xs :~: ys) => Rec el f xs -> Rec el f ys -> Bool
x ~= y = x == (cast y)


