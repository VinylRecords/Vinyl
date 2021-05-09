{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
#endif
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Constant-time field accessors for extensible records. The
-- trade-off is the usual lists vs arrays one: it is fast to add an
-- element to the head of a list, but element access is linear time;
-- array access time is uniform, but extending the array is more
-- slower.
module Data.Vinyl.ARec.Internal
  ( ARec (..)
  , IndexableField
  , toARec
  , fromARec
  , aget
  , unsafeAput
  , unsafeAlens
  , arecGetSubset
  , arecSetSubset
  , arecRepsMatchCoercion
  , arecConsMatchCoercion
  ) where
import Data.Vinyl.Core
import Data.Vinyl.Lens (RecElem(..), RecSubset(..))
import Data.Vinyl.TypeLevel

import qualified Data.Array as Array
import qualified Data.Array.Base as BArray
import GHC.Exts (Any)
import Unsafe.Coerce
#if __GLASGOW_HASKELL__ < 806
import Data.Constraint.Forall (Forall)
#endif
import Data.Coerce (Coercible)
import Data.Type.Coercion (Coercion (..))

-- | An array-backed extensible record with constant-time field
-- access.
newtype ARec (f :: k -> *) (ts :: [k]) = ARec (Array.Array Int Any)
type role ARec representational nominal

-- | Given that @xs@ and @ys@ have the same length, and mapping
-- @f@ over @xs@ and @g@ over @ys@ produces lists whose elements
-- are pairwise 'Coercible', @ARec f xs@ and @ARec g ys@ are
-- 'Coercible'.
arecRepsMatchCoercion :: AllRepsMatch f xs g ys => Coercion (ARec f xs) (ARec g ys)
arecRepsMatchCoercion = Coercion

-- | Given that @forall x. Coercible (f x) (g x)@, produce a coercion from
-- @ARec f xs@ to @ARec g xs@. While the constraint looks a lot like
-- @Coercible f g@, it is actually weaker.

#if __GLASGOW_HASKELL__ >= 806
arecConsMatchCoercion ::
  (forall (x :: k). Coercible (f x) (g x)) => Coercion (ARec f xs) (ARec g xs)
arecConsMatchCoercion = Coercion
#else
arecConsMatchCoercion :: forall k (f :: k -> *) (g :: k -> *) (xs :: [k]).
  Forall (Similar f g) => Coercion (Rec f xs) (Rec g xs)
-- Why do we need this? No idea, really. I guess some change in
-- newtype handling for Coercible in 8.6?
arecConsMatchCoercion = unsafeCoerce (Coercion :: Coercion (Rec f xs) (Rec f xs))
#endif

{-
-- This is sensible, but the ergonomics are likely quite bad thanks to the
-- interaction between Coercible resolution and resolution in the presence of
-- quantified constraints. Is there a good way to do this?

arecConsMatchCoercible :: forall k f g rep (r :: TYPE rep).
     (forall (x :: k). Coercible (f x) (g x))
  => ((forall (xs :: [k]). Coercible (ARec f xs) (ARec g xs)) => r) -> r
arecConsMatchCoercible f = f
-}

-- | Convert a 'Rec' into an 'ARec' for constant-time field access.
toARec :: forall f ts. (NatToInt (RLength ts)) => Rec f ts -> ARec f ts
toARec = go id
  where go :: ([Any] -> [Any]) -> Rec f ts' -> ARec f ts
        go acc RNil = ARec $! Array.listArray (0, n - 1) (acc [])
        go acc (x :& xs) = go (acc . (unsafeCoerce x :)) xs
        n = natToInt @(RLength ts)
{-# INLINE toARec #-}

-- | Defines a constraint that lets us index into an 'ARec' in order
-- to produce a 'Rec' using 'fromARec'.
class (NatToInt (RIndex t ts)) => IndexableField ts t where
instance (NatToInt (RIndex t ts)) => IndexableField ts t where

-- | Convert an 'ARec' into a 'Rec'.
fromARec :: forall f ts.
            (RecApplicative ts, RPureConstrained (IndexableField ts) ts)
         => ARec f ts -> Rec f ts
fromARec (ARec arr) = rpureConstrained @(IndexableField ts) aux
  where aux :: forall t. NatToInt (RIndex t ts) => f t
        aux = unsafeCoerce (arr Array.! natToInt @(RIndex t ts))
{-# INLINE fromARec #-}

-- | Get a field from an 'ARec'.
aget :: forall t f ts. (NatToInt (RIndex t ts)) => ARec f ts -> f t
aget (ARec arr) =
  unsafeCoerce (BArray.unsafeAt arr (natToInt @(RIndex t ts)))
{-# INLINE aget #-}

-- | Set a field in an 'ARec'.
unsafeAput :: forall t t' f ts ts'. (NatToInt (RIndex t ts))
      => f t' -> ARec f ts -> ARec f ts'
unsafeAput x (ARec arr) = ARec (arr Array.// [(i, unsafeCoerce x)])
  where i = natToInt @(RIndex t ts)
{-# INLINE unsafeAput #-}

-- | Define a lens for a field of an 'ARec'.
unsafeAlens :: forall f g t t' ts ts'. (Functor g, NatToInt (RIndex t ts))
      => (f t -> g (f t')) -> ARec f ts -> g (ARec f ts')
unsafeAlens f ar = fmap (flip (unsafeAput @t) ar) (f (aget ar))
{-# INLINE unsafeAlens #-}

-- instance (i ~ RIndex t ts, i ~ RIndex t' ts', NatToInt (RIndex t ts)) => RecElem ARec t t' ts ts' i where
--   rlens = alens
--   rget = aget
--   rput = aput

instance RecElem ARec t t' (t ': ts) (t' ': ts) 'Z where
  rlensC = unsafeAlens
  {-# INLINE rlensC #-}
  rgetC = aget
  {-# INLINE rgetC #-}
  rputC = unsafeAput @t
  {-# INLINE rputC #-}

instance (RIndex t (s ': ts) ~ 'S i, NatToInt i,  RecElem ARec t t' ts ts' i)
  => RecElem ARec t t' (s ': ts) (s ': ts') ('S i) where
  rlensC = unsafeAlens
  {-# INLINE rlensC #-}
  rgetC = aget
  {-# INLINE rgetC #-}
  rputC = unsafeAput @t
  {-# INLINE rputC #-}

-- | Get a subset of a record's fields.
arecGetSubset :: forall rs ss f.
                 (IndexWitnesses (RImage rs ss), NatToInt (RLength rs))
              => ARec f ss -> ARec f rs
arecGetSubset (ARec arr) = ARec (Array.listArray (0, n-1) $
                                 go (indexWitnesses @(RImage rs ss)))
  where go :: [Int] -> [Any]
        go = map (arr Array.!)
        n = natToInt @(RLength rs)
{-# INLINE arecGetSubset #-}

-- | Set a subset of a larger record's fields to all of the fields of
-- a smaller record.
arecSetSubset :: forall rs ss f. (IndexWitnesses (RImage rs ss))
              => ARec f ss -> ARec f rs -> ARec f ss
arecSetSubset (ARec arrBig) (ARec arrSmall) = ARec (arrBig Array.// updates)
  where updates = zip (indexWitnesses @(RImage rs ss)) (Array.elems arrSmall)
{-# INLINE arecSetSubset #-}

instance (is ~ RImage rs ss, IndexWitnesses is, NatToInt (RLength rs))
         => RecSubset ARec rs ss is where
  rsubsetC f big = fmap (arecSetSubset big) (f (arecGetSubset big))
  {-# INLINE rsubsetC #-}

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Show (Rec f rs)) => Show (ARec f rs) where
  show = show . fromARec

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Eq (Rec f rs)) => Eq (ARec f rs) where
  x == y = fromARec x == fromARec y

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Ord (Rec f rs)) => Ord (ARec f rs) where
  compare x y = compare (fromARec x) (fromARec y)
