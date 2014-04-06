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
import Data.Vinyl.Derived
import Data.Vinyl.TyFun
import Data.Vinyl.Witnesses
import Data.Vinyl.Idiom.Identity

import Data.Singletons
import Control.Applicative

-- | Project a field from a 'Rec'.
rGet' :: IElem r rs => Sing r -> Rec el f rs -> f (el $ r)
rGet' r = getConst . rLens' r Const
{-# INLINE rGet' #-}

-- | Project a field from a 'PlainRec'.
rGet :: IElem r rs => Sing r -> PlainRec el rs -> el $ r
rGet = (runIdentity .) . rGet'
{-# INLINE rGet #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
rPut' :: IElem r rs => Sing r -> f (el $ r) -> Rec el f rs -> Rec el f rs
rPut' r x = runIdentity . rLens' r (Identity . const x)
{-# INLINE rPut' #-}

-- | Set a field in a 'PlainRec'.
rPut :: IElem r rs => Sing r -> el $ r -> PlainRec el rs -> PlainRec el rs
rPut r x = rPut' r (Identity x)
{-# INLINE rPut #-}

-- | Modify a field.
rMod :: (IElem r rs, Functor f) => Sing r -> (el $ r -> el $ r) -> Rec el f rs -> Rec el f rs
rMod r f = runIdentity . rLens' r (Identity . fmap f)
{-# INLINE rMod #-}

-- We manually unroll several levels of 'Elem' value traversal to help
-- GHC statically index into small records.

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLens' :: IElem r rs => Sing r -> Lens' (Rec rs f) (f (El r))
rLens' :: forall r rs f g el. (IElem r rs, Functor g) => Sing r -> (f (el $ r) -> g (f (el $ r))) -> Rec el f rs -> g (Rec el f rs)
rLens' _ f = go implicitly
  where go :: Elem r rr -> Rec el f rr -> g (Rec el f rr)
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

        go' :: Elem r rr -> Rec el f rr -> g (Rec el f rr)
        go' Here (x :& xs) = fmap (:& xs) (f x)
        go' (There p) (x :& xs) = fmap (x :&) (go p xs)
        {-# INLINABLE go' #-}
{-# INLINE rLens' #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not
-- supported. In the parlance of the @lens@ package,
--
-- > rLens :: IElem r rs => Sing r -> Lens' (PlainRec el rs) (el $ r)
rLens :: forall r rs g el. (IElem r rs, Functor g) => Sing r -> (el $ r -> g (el $ r)) -> PlainRec el rs -> g (PlainRec el rs)
rLens r = rLens' r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE rLens #-}
