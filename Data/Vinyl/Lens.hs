{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- | Lenses into record fields.
module Data.Vinyl.Lens
  ( RecElem(..)
  , RElem
  , RecSubset(..)
  , RSubset
  , REquivalent
  , type (∈)
  , type (⊆)
  , type (≅)
  , type (<:)
  , type (:~:)
  ) where

import Data.Kind (Constraint)
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Typeable (Proxy(..))

-- | The presence of a field in a record is witnessed by a lens into its value.
-- The third parameter to 'RElem', @i@, is there to help the constraint solver
-- realize that this is a decidable predicate with respect to the judgemental
-- equality in @k@.
class i ~ RIndex r rs => RecElem record (r :: k) (rs :: [k]) (i :: Nat) where
  -- | An opportunity for instances to generate constraints based on
  -- the functor parameter of records passed to class methods.
  type RecElemFCtx record (f :: k -> *) :: Constraint
  type RecElemFCtx record f = ()

  -- | We can get a lens for getting and setting the value of a field which is
  -- in a record. As a convenience, we take a proxy argument to fix the
  -- particular field being viewed. These lenses are compatible with the @lens@
  -- library. Morally:
  --
  -- > rlens :: sing r => Lens' (Rec f rs) (f r)
  rlens
    :: (Functor g, RecElemFCtx record f)
    => sing r
    -> (f r -> g (f r))
    -> record f rs
    -> g (record f rs)

  -- | For Vinyl users who are not using the @lens@ package, we provide a getter.
  rget
    :: RecElemFCtx record f
    => sing r
    -> record f rs
    -> f r

  -- | For Vinyl users who are not using the @lens@ package, we also provide a
  -- setter. In general, it will be unambiguous what field is being written to,
  -- and so we do not take a proxy argument here.
  rput
    :: RecElemFCtx record f
    => f r
    -> record f rs
    -> record f rs

-- | 'RecElem' for classic vinyl 'Rec' types.
type RElem = RecElem Rec

-- This is an internal convenience stolen from the @lens@ library.
lens
  :: Functor f
  => (s -> a)
  -> (s -> b -> t)
  -> (a -> f b)
  -> s
  -> f t
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

instance RecElem Rec r (r ': rs) 'Z where
  rlens _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rlens #-}
  rget k = getConst . rlens k Const
  {-# INLINE rget #-}
  rput y = getIdentity . rlens Proxy (\_ -> Identity y)
  {-# INLINE rput #-}

instance (RIndex r (s ': rs) ~ 'S i, RElem r rs i) => RecElem Rec r (s ': rs) ('S i) where
  rlens p f (x :& xs) = fmap (x :&) (rlens p f xs)
  {-# INLINE rlens #-}
  rget k = getConst . rlens k Const
  {-# INLINE rget #-}
  rput y = getIdentity . rlens Proxy (\_ -> Identity y)
  {-# INLINE rput #-}

-- | If one field set is a subset another, then a lens of from the latter's
-- record to the former's is evident. That is, we can either cast a larger
-- record to a smaller one, or we may replace the values in a slice of a
-- record.
class is ~ RImage rs ss => RecSubset record (rs :: [k]) (ss :: [k]) is where
  -- | An opportunity for instances to generate constraints based on
  -- the functor parameter of records passed to class methods.
  type RecSubsetFCtx record (f :: k -> *) :: Constraint
  type RecSubsetFCtx record f = ()

  -- | This is a lens into a slice of the larger record. Morally, we have:
  --
  -- > rsubset :: Lens' (Rec f ss) (Rec f rs)
  rsubset
    :: (Functor g, RecSubsetFCtx record f)
    => (record f rs -> g (record f rs))
    -> record f ss
    -> g (record f ss)

  -- | The getter of the 'rsubset' lens is 'rcast', which takes a larger record
  -- to a smaller one by forgetting fields.
  rcast
    :: RecSubsetFCtx record f
    => record f ss
    -> record f rs
  rcast = getConst . rsubset Const
  {-# INLINE rcast #-}

  -- | The setter of the 'rsubset' lens is 'rreplace', which allows a slice of
  -- a record to be replaced with different values.
  rreplace
    :: RecSubsetFCtx record f
    => record f rs
    -> record f ss
    -> record f ss
  rreplace rs = getIdentity . rsubset (\_ -> Identity rs)
  {-# INLINE rreplace #-}

type RSubset = RecSubset Rec

instance RecSubset Rec '[] ss '[] where
  rsubset = lens (const RNil) const

instance (RElem r ss i , RSubset rs ss is) => RecSubset Rec (r ': rs) ss (i ': is) where
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
