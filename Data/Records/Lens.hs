{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Data.Records.Lens
  ( module Control.Lens
  , RLens
  , rLens
  , rGet
  , rPut
  , rMod
  ) where

import Data.Records.Rec
import Data.Records.Field
import Data.Records.Proofs

import Control.Lens

type RLens sy t = IElem (sy ::: t) fs => SimpleLens (Rec fs) t

rLens :: (sy ::: t) -> RLens sy t
rLens f = rLens' f implicitly

rGet = view . rLens
rPut = flip . set . rLens
rMod = flip . over . rLens

-- Records have lenses
rLens' :: (f ~ (sy ::: t)) => f -> Elem f fs -> SimpleLens (Rec fs) t
rLens' _ Here = lens (\((_,x) :& xs) -> x) (\((k,_) :& xs) x -> (k,x) :& xs)
rLens' f (There p) = rLensPrepend $ rLens' f p

rLensPrepend :: SimpleLens (Rec fs) t -> SimpleLens (Rec (f ': fs)) t
rLensPrepend l = lens (\(_ :& xs) -> view l xs) (\(a :& xs) x -> a :& (set l x xs))

