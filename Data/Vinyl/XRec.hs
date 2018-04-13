{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A variant of 'Rec' whose values have eliminated common syntactic
-- clutter due to 'Identity', 'Compose', and 'ElField' type
-- constructors.
--
-- A common pain point with using 'Rec' is the mandatory /context/ of
-- each value. A basic record might look like this, @Identity "joe" :&
-- Identity 23 :& RNil :: Rec Identity '[String, Int]@. The 'Identity'
-- constructors are a nuisance, so we offer a way of avoiding them:
-- @"joe" ::& 23 ::& XRNil :: XRec Identity '[String,Int]@. Facilities
-- are provided for converting between 'XRec' and 'Rec' so that the
-- 'Rec' API is available even if you choose to use 'XRec' for
-- construction or pattern matching.
module Data.Vinyl.XRec where
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor
import GHC.TypeLits (KnownSymbol)

-- | The so-called /higher-kinded data/ type family. See <http://reasonablypolymorphic.com/blog/higher-kinded-data>
--
-- The mappings provided are:
-- @'Identity' a ==> a@
-- @('Compose' f g) a ==> f (g a)@
-- @'Field' a ==> a@
type family HKD (f :: u -> *) (t :: u) :: * where
  HKD Identity a = a
  HKD (Compose f g) a = HKD f (HKD g a)
  HKD ElField '(s,a) = a
  HKD f a = f a

-- | A 'Rec' whose fields have common context clutter eliminated by
-- 'HKD'.
data XRec :: (u -> *) -> [u] -> * where
  XRNil :: XRec f '[]
  (::&) :: !(HKD f r) -> !(XRec f rs) -> XRec f (r ': rs)
infixr 7 ::&

-- | Mapping each HKD functor to a symbol allows us to have a clear
-- catch-all case in a closed type family. Without this restriction to
-- a finite type, we would require overlapping type class instances.
data IsoHKDSym = IsoId | IsoComp | IsoElF | IsoNull

-- | Map a record context to the appropriate 'IsoHKDSym'
type family HasIsoHKDSym (t :: k) :: IsoHKDSym where
  HasIsoHKDSym Identity = 'IsoId
  HasIsoHKDSym (Compose f g) = 'IsoComp
  HasIsoHKDSym ElField = 'IsoElF
  HasIsoHKDSym f = 'IsoNull

-- | Witnesses for the isomorphisms between type constructors and
-- their HKD mappings.
class HasIsoHKDSym f ~ tag => IsoHKD (tag :: IsoHKDSym) f a where
  unHKD :: HKD f a -> f a
  toHKD :: f a -> HKD f a

-- | We freely move between @Identity x@ and @x@.
instance IsoHKD 'IsoId Identity a where
  unHKD = Identity
  toHKD = getIdentity

-- | We freely move between @(f :. g) x@ and @f (g x)@.
instance ( IsoHKD (HasIsoHKDSym f) f (HKD g a)
         , IsoHKD (HasIsoHKDSym g) g a
         , Functor f)
         => IsoHKD 'IsoComp (Compose f g) a where
  unHKD x = Compose (unHKD <$> unHKD x)
  toHKD (Compose fgx) = toHKD (toHKD <$> fgx)

-- | We freely move between @Field x@ and @x@.
instance KnownSymbol s => IsoHKD 'IsoElF ElField '(s,a) where
  unHKD = Field
  toHKD (Field x) = x

-- | All other contexts are opaque; this is reflexivity: we move
-- between @f x@ and @f x@.
instance (HKD f a ~ f a, HasIsoHKDSym f ~ 'IsoNull) => IsoHKD 'IsoNull f a where
  unHKD = id
  toHKD = id

-- | Conversion between 'XRec' and 'Rec'. It is convenient to build
-- and consume 'XRec' values to reduce syntactic noise, but 'Rec' has
-- a richer API that is difficult to build around the 'HKD' type
-- family.
class IsoXRec f ts where
  fromXRec :: XRec f ts -> Rec f ts
  toXRec :: Rec f ts -> XRec f ts

instance IsoXRec f '[] where
  fromXRec XRNil = RNil
  toXRec RNil = XRNil

instance (IsoXRec f ts, IsoHKD (HasIsoHKDSym f) f t) => IsoXRec f (t ': ts) where
  fromXRec (x ::& xs) = unHKD x :& fromXRec xs
  toXRec (x :& xs) = toHKD x ::& toXRec xs
