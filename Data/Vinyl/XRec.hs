{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Vinyl.Lens (RecElem, RecElemFCtx, rgetC)
import Data.Vinyl.TypeLevel (RIndex)
import Data.Monoid
import GHC.TypeLits (KnownSymbol)

type XRec f = Rec (XData f)
pattern (::&) :: HKD f r -> XRec f rs -> XRec f (r ': rs)
pattern x ::& xs = XData x :& xs
{-# COMPLETE (::&) #-}

infixr 7 ::&

pattern XRNil :: XRec f '[]
pattern XRNil = RNil
{-# COMPLETE XRNil #-}

-- | Like 'rmap', but the supplied function is written against the
-- 'HKD'-simplified types. This is 'xrmap' sandwiched in between
-- 'fromXRec' and 'toXRec'.
rmapX :: forall f g rs. (XRMap f g rs, IsoXRec f rs, IsoXRec g rs)
      => (forall a. HKD f a -> HKD g a) -> Rec f rs -> Rec g rs
rmapX f = fromXRec . xrmapAux aux . toXRec
  where aux :: forall a. XData f a -> XData g a
        aux = XData . f @a . unX

-- | This is 'rmapX' specialized to a type at which it does not change
-- interpretation functor. This can help with type inference.
rmapXEndo :: forall f rs. (XRMap f f rs, IsoXRec f rs)
          => (forall a. HKD f a -> HKD f a) -> Rec f rs -> Rec f rs
rmapXEndo f = fromXRec . xrmapAux aux . toXRec
  where aux :: forall a. XData f a -> XData f a
        aux = XData . f @a . unX

-- | This is 'rmap' for 'XRec'. We apply a natural transformation
-- between interpretation functors to transport a record value between
-- interpretations.
xrmap :: forall f g rs. XRMap f g rs
      => (forall a. HKD f a -> HKD g a) -> XRec f rs -> XRec g rs
xrmap f = xrmapAux aux
  where aux :: forall a. XData f a -> XData g a
        aux = XData . f @a . unX

-- | A wrapper for an 'HKD'-simplified value. That is, noisy value
-- constructors like 'Identity' and 'Compose' are ellided. This is
-- used in the 'xrmapAux' type class method, but may be ignored by
-- users whose needs are met by 'xrmap' and 'rmapX'.
newtype XData t a = XData { unX :: HKD t a }

-- | The implementation of 'xrmap' is broken into a type class to
-- permit unrolling of the recursion across a record. The function
-- mapped across the vector hides the 'HKD' type family under a newtype
-- constructor to help the type checker.
class XRMap (f :: u -> *) (g :: u -> *) (rs :: [u]) where
  xrmapAux :: (forall (a :: u) . XData f a -> XData g a) -> XRec f rs -> XRec g rs

instance XRMap f g '[] where
  xrmapAux _ RNil = RNil

instance forall f g r rs. (XRMap f g rs, IsoHKD f r, IsoHKD g r)
  => XRMap f g (r ': rs) where
  xrmapAux f (x :& xs) = f x :& xrmapAux f xs

-- | Like 'rapply': record of components @f r -> g r@ may be applied
-- to a record of @f@ to get a record of @g@.
class XRApply f g rs where
  xrapply :: XRec (Lift (->) f g) rs -> XRec f rs -> XRec g rs

instance XRApply f g '[] where
  xrapply RNil RNil = RNil

instance XRApply f g rs => XRApply f g (r ': rs) where
  xrapply (XData f :& fs) (XData x :& xs) = XData (f x) :& xrapply fs xs

-- | Conversion between 'XRec' and 'Rec'. It is convenient to build
-- and consume 'XRec' values to reduce syntactic noise, but 'Rec' has
-- a richer API that is difficult to build around the 'HKD' type
-- family.
class IsoXRec f ts where
  fromXRec :: XRec f ts -> Rec f ts
  toXRec :: Rec f ts -> XRec f ts

instance IsoXRec f '[] where
  fromXRec RNil = RNil
  toXRec RNil = XRNil

instance (IsoXRec f ts, IsoHKD f t) => IsoXRec f (t ': ts) where
  fromXRec (x ::& xs) = unHKD x :& fromXRec xs
  toXRec (x :& xs) = toHKD x ::& toXRec xs

-- | Isomorphism between a syntactically noisy value and a concise
-- one. For types like, 'Identity', we prefer to work with values of
-- the underlying type without writing out the 'Identity'
-- constructor. For @'Compose' f g a@, aka @(f :. g) a@, we prefer to
-- work directly with values of type @f (g a)@.
--
-- This involves the so-called /higher-kinded data/ type family. See
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data> for more
-- discussion.
class IsoHKD (f :: u -> *) (a :: u) where
  type HKD f a
  type HKD f a = f a
  unHKD :: HKD f a -> f a
  default unHKD :: HKD f a ~ f a => HKD f a -> f a
  unHKD = id
  toHKD :: f a -> HKD f a
  default toHKD :: (HKD f a ~ f a) => f a -> HKD f a
  toHKD = id

-- | Work with values of type 'Identity' @a@ as if they were simple of
-- type @a@.
instance IsoHKD Identity a where
  type HKD Identity a = a
  unHKD = Identity
  toHKD (Identity x) = x

-- | Work with values of type 'ElField' @'(s,a)@ as if they were of
-- type @a@.
instance KnownSymbol s => IsoHKD ElField '(s,a) where
  type HKD ElField '(s,a) = a
  unHKD = Field
  toHKD (Field x) = x

-- | Work with values of type 'Compose' @f g a@ as if they were of
-- type @f (g a)@.
instance (IsoHKD f (HKD g a), IsoHKD g a, Functor f) => IsoHKD (Compose f g) a where
  type HKD (Compose f g) a = HKD f (HKD g a)
  unHKD x = Compose (unHKD <$> unHKD x)
  toHKD (Compose fgx) = toHKD (toHKD <$> fgx)

-- | Work with values of type 'Lift' @(->) f g a@ as if they were of
-- type @f a -> g a@.
instance (IsoHKD f a, IsoHKD g a) => IsoHKD (Lift (->) f g) a where
  type HKD (Lift (->) f g) a = HKD f a -> HKD g a
  unHKD x = Lift (unHKD . x . toHKD)
  toHKD (Lift x) = toHKD . x . unHKD

instance IsoHKD IO a where
instance IsoHKD (Either a) b where
instance IsoHKD Maybe a where
instance IsoHKD First a where
instance IsoHKD Last a where
instance IsoHKD ((,) a) b where

-- | Work with values of type 'Sum' @a@ as if they were of type @a@.
instance IsoHKD Sum a where
  type HKD Sum a = a
  unHKD = Sum
  toHKD (Sum x) = x

-- | Work with values of type 'Product' @a@ as if they were of type @a@.
instance IsoHKD Product a where
  type HKD Product a = a
  unHKD = Product
  toHKD (Product x) = x

-- | Record field getter that pipes the field value through 'HKD' to
-- eliminate redundant newtype wrappings. Usage will typically involve
-- a visible type application to the field type. The definition is
-- similar to, @getHKD = toHKD . rget@.
rgetX :: forall a record f rs.
         (RecElem record a a rs rs (RIndex a rs),
          RecElemFCtx record f,
          IsoHKD f a)
      => record f rs -> HKD f a
rgetX = toHKD . rgetAux @a
  where rgetAux :: forall r.
                   (RecElem record r r rs rs (RIndex r rs),
                    RecElemFCtx record f)
                => record f rs -> f r
        rgetAux = rgetC
