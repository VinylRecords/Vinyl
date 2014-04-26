{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vinyl.Universe.Id (Id) where

import Data.Vinyl.TyFun

data Id :: (TyFun k k) -> * where
  Id :: Id el
type instance Id $ x = x

