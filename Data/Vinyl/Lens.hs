{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Lens
  ( type (∈)
  , type (⊆)
  , type (≅)
  , type (<:)
  , type (:~:)
  , RElem(..)
  , RSubset(..)
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Typeable (Proxy(..))

class i ~ RIndex r rs => RElem (r :: k) (rs :: [k]) (i :: Nat) where
  rlens
    :: Functor g
    => sing r
    -> (f r -> g (f r))
    -> Rec f rs
    -> g (Rec f rs)

  rget
    :: sing r
    -> Rec f rs
    -> f r
  rget k = getConst . rlens k Const

  rput
    :: sing r
    -> f r
    -> Rec f rs
    -> Rec f rs
  rput k y = getIdentity . rlens k (\_ -> Identity y)

lens
  :: Functor f
  => (t -> s)
  -> (t -> a -> b)
  -> (s -> f a)
  -> t
  -> f b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

instance RElem r (r ': rs) Z where
  rlens _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rlens #-}

instance (RIndex r (s ': rs) ~ S i, RElem r rs i) => RElem r (s ': rs) (S i) where
  rlens p f (x :& xs) = fmap (x :&) (rlens p f xs)
  {-# INLINE rlens #-}

class is ~ RImage rs ss => RSubset (rs :: [k]) (ss :: [k]) is where
  rsubset
    :: Functor g
    => (Rec f rs -> g (Rec f rs))
    -> Rec f ss
    -> g (Rec f ss)

  rcast
    :: Rec f ss
    -> Rec f rs
  rcast = getConst . rsubset Const
  {-# INLINE rcast #-}

  rreplace
    :: Rec f rs
    -> Rec f ss
    -> Rec f ss
  rreplace rs = getIdentity . rsubset (\_ -> Identity rs)
  {-# INLINE rreplace #-}

instance RSubset '[] ss '[] where
  rsubset = lens (const RNil) const

instance (RElem r ss i , RSubset rs ss is) => RSubset (r ': rs) ss (i ': is) where
  rsubset = lens (\ss -> rget Proxy ss :& rcast ss) set
    where
      set :: Rec f ss -> Rec f (r ': rs) -> Rec f ss
      set ss (r :& rs) = rput Proxy r $ rreplace rs ss

type REquivalent rs ss is js = (RSubset rs ss is, RSubset ss rs js)

type r ∈ rs = RElem r rs (RIndex r rs)
type rs ⊆ ss = RSubset rs ss (RImage rs ss)
type rs ≅ ss = REquivalent rs ss (RImage rs ss) (RImage ss rs)
type rs <: ss = rs ⊆ ss
type rs :~: ss = rs ≅ ss
