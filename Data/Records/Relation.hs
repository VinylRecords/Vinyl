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

class ss <: ts where
  cast :: Rec ss -> Rec ts

instance xs <: '[] where
  cast _ = RNil

instance (y ~ (sy ::: t), IElem y xs, xs <: ys) => xs <: (y ': ys) where
  cast r = (field, rGet field r) :& cast r
    where field = lookupField implicitly r

lookupField :: Elem x xs -> Rec xs -> x
lookupField Here ((k,_) :& _) = k
lookupField (There p) (_ :& xs) = lookupField p xs


