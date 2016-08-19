{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Index where

import Data.Vinyl.Core
import Data.Vinyl.Functor

-- | A Peano index provides a constructive proof that the type-level list @rs@
-- contains the type @r@.
data RIndex (rs :: [k]) (r :: k) where
  Z :: RIndex (r ': rs) r
  S :: RIndex rs r -> RIndex (s ': rs) r

rlensIx
  :: Functor g
  => RIndex rs r
  -> (f r     -> g (f r))
  -> Rec f rs -> g (Rec f rs)
rlensIx Z f (r:&rs) = fmap (:& rs) (f r)
rlensIx (S ix) f (r:&rs) = fmap (r :&) (rlensIx ix f rs)
{-# INLINABLE rlensIx #-}

rgetIx :: RIndex rs r -> Rec f rs -> f r
rgetIx ix = getConst . rlensIx ix Const
{-# INLINABLE rgetIx #-}

(!) :: Rec f rs -> RIndex rs r -> f r
infixl 9 !
(!) = flip rgetIx

rputIx :: RIndex rs r -> f r -> Rec f rs -> Rec f rs
rputIx ix r = getIdentity . rlensIx ix (\_ -> Identity r)
{-# INLINABLE rputIx #-}

class Rid rs where rid :: Rec (RIndex rs) rs
instance Rid '[] where rid = RNil
instance Rid rs => Rid (r ': rs) where rid = Z :& rmap S rid

-- | specializes as composition of records of indices
rgetIxs :: Rec (RIndex rs) ss -> Rec f rs -> Rec f ss
rgetIxs ixs rec = rmap (rec !) ixs

rputIxs :: Rec (RIndex rs) ss -> Rec f rs -> Rec f ss -> Rec f rs
rputIxs RNil rs _ = rs
rputIxs (ix:&ixs) rs (s:&ss) = rputIx ix s (rputIxs ixs rs ss)

rlenseIxs
  :: Functor g
  => Rec (RIndex rs) ss
  -> (Rec f ss -> g (Rec f ss))
  ->  Rec f rs -> g (Rec f rs)
rlenseIxs ixs = lens (rgetIxs ixs) (rputIxs ixs)
  where
    lens sa sbt afb s = fmap (sbt s) $ afb (sa s)

class RElem (r :: k) (rs :: [k]) where relemIndex :: RIndex rs r
instance {-# OVERLAPPING #-} r `RElem` (r ': rs) where relemIndex = Z
instance {-# OVERLAPPABLE #-} (r `RElem` rs)
  => r `RElem` (r' ': rs) where relemIndex = S relemIndex

rlens
  :: (RElem r rs , Functor g)
  => sing r
  -> (f r     -> g (f r))
  -> Rec f rs -> g (Rec f rs)
rlens _ = rlensIx relemIndex
{-# INLINE rlens #-}

rget :: RElem r rs => sing r -> Rec f rs -> f r
rget _ = rgetIx relemIndex
{-# INLINE rget #-}

rput :: RElem r rs => f r -> Rec f rs -> Rec f rs
rput = rputIx relemIndex
{-# INLINE rput #-}

class RSubset (ss :: [k]) (rs :: [k]) where rsubIndices :: Rec (RIndex rs) ss
instance '[] `RSubset` rs where rsubIndices = RNil
instance (RElem r ss , RSubset rs ss) => RSubset (r ': rs) ss where
  rsubIndices = relemIndex :& rsubIndices

rsubset
  :: (Functor g, RSubset ss rs)
  => (Rec f ss -> g (Rec f ss))
  ->  Rec f rs -> g (Rec f rs)
rsubset = rlenseIxs rsubIndices

rcast :: RSubset ss rs => Rec f rs -> Rec f ss
rcast = rgetIxs rsubIndices

rreplace :: RSubset ss rs => Rec f rs -> Rec f ss -> Rec f rs
rreplace = rputIxs rsubIndices

-- | Two record types are equivalent when they are subtypes of each other.
type REquivalent rs ss = (RSubset rs ss, RSubset ss rs)

-- | A shorthand for 'RElem' which supplies its index.
type r ∈ rs = RElem r rs

-- | A shorthand for 'RSubset' which supplies its image.
type rs ⊆ ss = RSubset rs ss

-- | A shorthand for 'REquivalent' which supplies its images.
type rs ≅ ss = REquivalent rs ss

-- | A non-unicode equivalent of @(⊆)@.
type rs <: ss = rs ⊆ ss

-- | A non-unicode equivalent of @(≅)@.
type rs :~: ss = rs ≅ ss
