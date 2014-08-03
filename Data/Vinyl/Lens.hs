{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields. Lenses produced with 'rlens' are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens where

import Data.Vinyl.Core
import Data.Vinyl.Derived
import Data.Vinyl.TyFun
import Data.Vinyl.Idiom.Identity

import Control.Applicative

-- | Project a field from a 'Rec'.
rGet' :: (r ∈ rs) => sing r -> Rec el f rs -> f (el $ r)
rGet' r = getConst . rlens' r Const
{-# INLINE rGet' #-}

-- | Project a field from a 'PlainRec'.
rGet :: (r ∈ rs) => sing r -> PlainRec el rs -> el $ r
rGet = (runIdentity .) . rGet'
{-# INLINE rGet #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
rPut' :: (r ∈ rs) => sing r -> f (el $ r) -> Rec el f rs -> Rec el f rs
rPut' r x = runIdentity . rlens' r (Identity . const x)
{-# INLINE rPut' #-}

-- | Set a field in a 'PlainRec'.
rPut :: (r ∈ rs) => sing r -> el $ r -> PlainRec el rs -> PlainRec el rs
rPut r x = rPut' r (Identity x)
{-# INLINE rPut #-}

-- | Modify a field.
rMod :: (r ∈ rs , Functor f) => sing r -> (el $ r -> el $ r) -> Rec el f rs -> Rec el f rs
rMod r f = runIdentity . rlens' r (Identity . fmap f)
{-# INLINE rMod #-}

