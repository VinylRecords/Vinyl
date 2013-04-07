{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Vinyl.Lens
  ( module Control.Lens
  , RLens
  , rLens
  , rGet
  , rPut
  , rMod
  ) where

import           Data.Vinyl.Field
import           Data.Vinyl.Rec
import           Data.Vinyl.Witnesses

import           Control.Lens
import           Control.Monad.Identity

type RLens sy t = IElem (sy ::: t) rs => SimpleLens (PlainRec rs) t

rLens :: (sy ::: t) -> RLens sy t
rLens f = rLens' f implicitly

rGet = view . rLens

rPut = set . rLens
rMod = over . rLens

-- Records have lenses
rLens' :: (r ~ (sy ::: t)) => r -> Elem r rs -> SimpleLens (PlainRec rs) t
rLens' _ Here = lens (\(x :& xs) -> runIdentity x) (\(_ :& xs) x -> Identity x :& xs)
rLens' f (There p) = rLensPrepend $ rLens' f p

rLensPrepend :: SimpleLens (PlainRec rs) t -> SimpleLens (PlainRec (l ': rs)) t
rLensPrepend l = lens (\(x :& xs) -> view l xs) (\(a :& xs) x -> a :& (set l x xs))
