{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Corecord where

import Data.Vinyl.TyFun
import Data.Vinyl.Witnesses

data CoRec :: (u -> *) -> (TyFun u * -> *) -> (* -> *) -> [u] -> * where
  (:>) :: (r ∈ rs) => sing r -> !(f (el $ r)) -> CoRec sing el f rs

