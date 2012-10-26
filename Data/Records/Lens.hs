{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Records.Lens
  ( Lens(..)
  , modify
  , RLens(..)
  , rLens
  , rGet
  , rPut
  ) where

import Data.Records.Rec
import Data.Records.Field
import Data.Records.Proofs

data Lens a b =
  Lens { get :: (a -> b)
       , put :: (b -> a -> a)
       }

modify :: Lens a b -> (b -> b) -> a -> a
modify l f x = put l (f (get l x)) x

type RLens sy t = IElem (sy ::: t) fs => Lens (Rec fs) t

rLens :: (sy ::: t) -> RLens sy t
rLens f = rLens' f implicitly

rGet = get . rLens
rPut = put . rLens

-- Records have lenses
rLens' :: f ~ (sy ::: t) => f -> Elem f fs -> Lens (Rec fs) t
rLens' _ Here =
  Lens { get = \((_,x) :& xs) -> x
       , put = \x ((k,_) :& xs) -> (k,x) :& xs
       }
rLens' f (There p) = rLensPrepend $ rLens' f p

rLensPrepend :: Lens (Rec fs) t -> Lens (Rec (f ': fs)) t
rLensPrepend (Lens g p) =
  Lens { get = \(_ :& xs) -> g xs
       , put = \x (a :& xs) -> a :& (p x xs)
       }

