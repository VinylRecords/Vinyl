{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Vinyl.Lens
  ( module Control.Lens
  , RLens
  , rLens
  , rGet
  , rPut
  , rMod
  ) where

import           Data.Vinyl.Field
import           Data.Vinyl.Rec
import           Data.Vinyl.Witnesses

import           Control.Lens
import           Control.Monad.Identity

type RLens sy t = IElem (sy ::: t) rs => Lens' (PlainRec rs) t

rLens :: (sy ::: t) -> RLens sy t
rLens f = rLens' f implicitly
{-# INLINE rLens #-}

rGet = view . rLens
{-# INLINE rGet #-}

rPut = set . rLens
{-# INLINE rPut #-}

rMod = over . rLens
{-# INLINE rMod #-}

-- We manually unroll several levels of record traversal via 'Elem'
-- values to help GHC eliminate the 'Implicit' dictionaries at
-- runtime.

-- Records have lenses
{-# INLINE rLens' #-}
rLens' :: forall r sy t rs. (r ~ (sy ::: t)) => r -> Elem r rs -> Lens' (PlainRec rs) t
rLens' _ = go
  where goHere :: Elem r rs' -> Lens' (PlainRec rs') t
        goHere Here = lens (\(x :& _) -> runIdentity x)
                           (\(_ :& xs) x -> Identity x :& xs)
        goHere _ = error "Unintended base case invocation"
        {-# INLINE go #-}
        go :: Elem r rs' -> Lens' (PlainRec rs') t
        go Here = goHere Here
        go (There Here) = rLensPrepend $ goHere Here
        go (There (There Here)) = rLensPrepend $ rLensPrepend $ goHere Here
        go (There (There (There Here))) = 
          rLensPrepend $ rLensPrepend $ rLensPrepend $ goHere Here
        go (There (There (There (There Here)))) = 
          rLensPrepend $ rLensPrepend $ rLensPrepend $ rLensPrepend
          $ goHere Here
        go (There (There (There (There p)))) = 
          rLensPrepend $ rLensPrepend $ rLensPrepend $ rLensPrepend $ go' p
        {-# INLINABLE go' #-}
        go' :: Elem r rs' -> Lens' (PlainRec rs') t
        go' Here = goHere Here
        go' (There p) = rLensPrepend $ go p
-- rLens' _ Here = lens (\(x :& xs) -> runIdentity x) (\(_ :& xs) x -> Identity x :& xs)
-- rLens' f (There p) = rLensPrepend $ rLens' f p

rLensPrepend :: Lens' (PlainRec rs) t -> Lens' (PlainRec (l ': rs)) t
rLensPrepend l = lens (\(_ :& xs) -> view l xs) (\(a :& xs) x -> a :& (set l x xs))
{-# INLINE rLensPrepend #-}
