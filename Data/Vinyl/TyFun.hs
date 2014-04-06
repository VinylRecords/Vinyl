{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.TyFun where

data TyFun :: * -> * -> *
type family (f :: TyFun k l -> *) $ (a :: k) :: l

data TC :: (k -> *) -> TyFun k * -> *
type instance TC t $ x = t x

