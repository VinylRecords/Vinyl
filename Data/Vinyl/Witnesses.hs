{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vinyl.Witnesses where

import GHC.Exts (Constraint)

class ErrMsg (err :: k) where
  neverEver :: forall f a. f err -> a

type family NotElemB (x :: k) (xs :: [k]) :: Bool where
  NotElemB x '[] = True
  NotElemB x (x ': xs) = False
  NotElemB x (y ': xs) = NotElemB x xs

type family (b :: Bool) ?? (err :: k) :: Constraint where
  True ?? err = ()
  False ?? err = ErrMsg err

type family AndB (b :: Bool) (b' :: Bool) :: Bool where
  AndB True True = True
  AndB True False = False
  AndB False True = False
  AndB False False = False

type family SetWFB (xs :: [k]) :: Bool where
  SetWFB '[] = True
  SetWFB (x ': xs) = AndB (NotElemB x xs) (SetWFB xs)

type SetWF (xs :: [k]) = SetWFB xs ?? '("The list", xs, "contains duplicates and is not a set")

class Implicit p where
  implicitly :: p

-- | An inductive list membership proposition.
data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

-- | A constraint for implicit resolution of list membership proofs.
type IElem x xs = (SetWF xs, Implicit (Elem x xs))
type x ∈ xs = IElem x xs

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There implicitly

