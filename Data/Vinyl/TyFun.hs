{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.TyFun where

data TyFun :: * -> * -> *
type family App (f :: TyFun k l -> *) (a :: k) :: l

data TC :: (k -> *) -> TyFun k * -> *
type instance App (TC t) x = t x
type f $ x = App f x
