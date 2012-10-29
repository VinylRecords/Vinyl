{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Records.Relation
  ( (<:)(..)
  , (:~:)
  , rIso
  ) where

import Data.Records.Proofs
import Data.Records.Field
import Data.Records.Rec
import Data.Records.Lens

-- A subtyping relation
class ss <: ts where
  cast :: Rec ss -> Rec ts

-- If two records types are subtypes of each other, that means that they
-- differ only in order of fields.
type ss :~: ts = (ss <: ts, ts <: ss)

instance xs <: '[] where
  cast _ = RNil

instance (y ~ (sy ::: t), IElem y xs, xs <: ys) => xs <: (y ': ys) where
  cast r = (field, rGet field r) :& cast r
    where field = lookupField implicitly r

lookupField :: Elem x xs -> Rec xs -> x
lookupField Here ((k,_) :& _) = k
lookupField (There p) (_ :& xs) = lookupField p xs

rIso :: (fs1 :~: fs2) => SimpleIso (Rec fs1) (Rec fs2)
rIso = iso cast cast


