{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

type RLens sy t = forall fs. IElem (sy ::: t) fs => SimpleLens (Rec fs) t

rLens :: (sy ::: t) -> RLens sy t
rLens f = rLens' f implicitly

rGet = view . rLens
rPut = flip . set . rLens
rMod = flip . over . rLens

-- Records have lenses
rLens' :: forall sy fs f t. (f ~ (sy ::: t)) => f -> Elem f fs -> SimpleLens (Rec fs) t
rLens' _ Here = lens ((\((_,x) :& xs) -> x) :: Rec fs -> t) ((\((k,_) :& xs) x -> (k,x) :& xs) :: Rec fs -> t -> Rec fs)
rLens' f (There p) = rLensPrepend $ rLens' f p

rLensPrepend :: forall fs f t. SimpleLens (Rec fs) t -> SimpleLens (Rec (f ': fs)) t
rLensPrepend l = lens ((\(_ :& xs) -> view l xs) :: Rec (f ': fs) -> t) ((\(a :& xs) x -> a :& (set l x xs)) :: Rec (f ': fs) -> t -> Rec (f ': fs))

