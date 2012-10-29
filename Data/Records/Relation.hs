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
  cast _ = Record

instance (y ~ (sy ::: t), IElem y xs, xs <: ys) => xs <: (y ': ys) where
  cast r = cast r :- field :=: rGet field r
    where field = lookupField implicitly r

lookupField :: Elem x xs -> Rec xs -> x
lookupField Here (_ :- k :=: _) = k
lookupField (There p) (xs :- _) = lookupField p xs


