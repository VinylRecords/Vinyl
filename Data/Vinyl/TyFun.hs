{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vinyl.TyFun where

data TyFun :: * -> * -> *
type family App (f :: TyFun k l -> *) (a :: k) :: l

data TC :: (k -> *) -> TyFun k * -> *
type instance App (TC t) x = t x
type f $ x = App f x

data Compose :: (TyFun b c -> *) -> (TyFun a b -> *) -> (TyFun a c) -> * where
  Compose :: Compose f g el

type (f :: b -> *) :. (g :: TyFun a b -> *) = Compose (TC f) g

type instance App (Compose f g) x = f $ (g $ x)
