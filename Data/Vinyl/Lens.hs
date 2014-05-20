{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields. Lenses produced with 'rLens' are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens where

import Data.Vinyl.Core
import Data.Vinyl.TyFun
import Data.Vinyl.Witnesses
import Data.Vinyl.Idiom.Identity

import Control.Applicative

-- | Project a field from a 'Rec'.
rGet :: (r ∈ rs) => sing r -> Rec el rs -> el $ r
rGet r = getConst . rLens r Const
{-# INLINE rGet #-}

-- | Set a field in a 'Rec'.
rPut :: (r ∈ rs) => sing r -> el $ r -> Rec el rs -> Rec el rs
rPut r x = runIdentity . rLens r (Identity . const x)
{-# INLINE rPut #-}

-- | Modify a field in a 'Rec'.
rMod :: (r ∈ rs) => sing r -> (el $ r -> el $ r) -> Rec el rs -> Rec el rs
rMod r f = runIdentity . rLens r (Identity . f)
{-# INLINE rMod #-}

-- We manually unroll several levels of 'Elem' value traversal to help
-- GHC statically index into small records.

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLens :: (r ∈ rs) => Sing r -> Lens' (Rec el rs) (el $ r)
rLens :: forall r rs g el sing. (r ∈ rs , Functor g) => sing r -> ((el $ r) -> g (el $ r)) -> Rec el rs -> g (Rec el rs)
rLens _ f = go implicitly
  where go :: Elem r rr -> Rec el rr -> g (Rec el rr)
        go Here (x :& xs) = fmap (:& xs) (f x)
        go (There Here) (a :& x :& xs) = fmap ((a :&) . (:& xs)) (f x)
        go (There (There Here)) (a :& b :& x :& xs) =
          fmap (\x' -> a :& b :& x' :& xs) (f x)
        go (There (There (There Here))) (a :& b :& c :& x :& xs) =
          fmap (\x' -> a :& b :& c :& x' :& xs) (f x)
        go (There (There (There (There Here)))) (a :& b :& c :& d :& x :& xs) =
          fmap (\x' -> a :& b :& c :& d :& x' :& xs) (f x)
        go (There (There (There (There p)))) (a :& b :& c :& d :& xs) =
          fmap (\xs' -> a :& b :& c :& d :& xs') (go' p xs)
        {-# INLINE go #-}

        go' :: Elem r rr -> Rec el rr -> g (Rec el rr)
        go' Here (x :& xs) = fmap (:& xs) (f x)
        go' (There p) (x :& xs) = fmap (x :&) (go p xs)
        {-# INLINABLE go' #-}
{-# INLINE rLens #-}

