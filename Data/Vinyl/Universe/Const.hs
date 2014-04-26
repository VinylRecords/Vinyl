{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vinyl.Universe.Const (Const) where

import Data.Vinyl.TyFun

data Const :: * -> (TyFun k *) -> * where
  Const :: Const t el

type instance Const t $ x = t
