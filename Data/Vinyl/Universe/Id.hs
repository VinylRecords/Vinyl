{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vinyl.Universe.Id (Id) where

import Data.Vinyl.TyFun

type family Id_ (x :: k) :: k
type instance Id_ x = x

data Id :: (TyFun k k) -> * where
  Id :: Id el
type instance Id $ n = Id_ n

