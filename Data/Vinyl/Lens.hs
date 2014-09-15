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
  ( RElem(..)
  , RSubset(..)
  , REquivalent
  , type (∈)
  , type (⊆)
  , type (≅)
  , type (<:)
  , type (:~:)
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Typeable (Proxy(..))

-- | The presence of a field in a record is witnessed by a lens into its value.
-- The third parameter to 'RElem', @i@, is there to help the constraint solver
-- realize that this is a decidable predicate with respect to the judgemental
-- equality in @k@.
class i ~ RIndex r rs => RElem (r :: k) (rs :: [k]) (i :: Nat) where

  -- | We can get a lens for getting and setting the value of a field which is
  -- in a record. As a convenience, we take a proxy argument to fix the
  -- particular field being viewed. These lenses are compatible with the @lens@
  -- library. Morally:
  --
  -- > rlens :: sing r => Lens' (Rec f rs) (f r)
  rlens
    :: Functor g
    => sing r
    -> (f r -> g (f r))
    -> Rec f rs
    -> g (Rec f rs)

  -- | For Vinyl users who are not using the @lens@ package, we provide a getter.
  rget
    :: sing r
    -> Rec f rs
    -> f r
  rget k = getConst . rlens k Const

  -- | For Vinyl users who are not using the @lens@ package, we also provide a
  -- setter. In general, it will be unambiguous what field is being written to,
  -- and so we do not take a proxy argument here.
  rput
    :: f r
    -> Rec f rs
    -> Rec f rs
  rput y = getIdentity . rlens Proxy (\_ -> Identity y)

-- This is an internal convenience stolen from the @lens@ library.
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

-- | If one field set is a subset another, then a lens of from the latter's
-- record to the former's is evident. That is, we can either cast a larger
-- record to a smaller one, or we may replace the values in a slice of a
-- record.
class is ~ RImage rs ss => RSubset (rs :: [k]) (ss :: [k]) is where

  -- | This is a lens into a slice of the larger record. Morally, we have:
  --
  -- > rsubset :: Lens' (Rec f ss) (Rec f rs)
  rsubset
    :: Functor g
    => (Rec f rs -> g (Rec f rs))
    -> Rec f ss
    -> g (Rec f ss)

  -- | The getter of the 'rsubset' lens is 'rcast', which takes a larger record
  -- to a smaller one by forgetting fields.
  rcast
    :: Rec f ss
    -> Rec f rs
  rcast = getConst . rsubset Const
  {-# INLINE rcast #-}

  -- | The setter of the 'rsubset' lens is 'rreplace', which allows a slice of
  -- a record to be replaced with different values.
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
      set ss (r :& rs) = rput r $ rreplace rs ss

-- | Two record types are equivalent when they are subtypes of each other.
type REquivalent rs ss is js = (RSubset rs ss is, RSubset ss rs js)

-- | A shorthand for 'RElem' which supplies its index.
type r ∈ rs = RElem r rs (RIndex r rs)

-- | A shorthand for 'RSubset' which supplies its image.
type rs ⊆ ss = RSubset rs ss (RImage rs ss)

-- | A shorthand for 'REquivalent' which supplies its images.
type rs ≅ ss = REquivalent rs ss (RImage rs ss) (RImage ss rs)

-- | A non-unicode equivalent of @(⊆)@.
type rs <: ss = rs ⊆ ss

-- | A non-unicode equivalent of @(≅)@.
type rs :~: ss = rs ≅ ss
