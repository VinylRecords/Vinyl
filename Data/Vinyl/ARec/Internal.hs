{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
#endif
{-# LANGUAGE RankNTypes #-}
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
--
-- Tradeoffs:
--
-- * No sharing of the spine (i.e. when you change elements in the front of the
--   record the tail can't be re-used)
-- * ARec requires (4 + n) words + size of the fields
--   * 1 for the ARec constructor
--   * 1 for the pointer to the SmallArray#
--   * The SmallArray# has 2 words as header (1 for GC, 1 for number of elements)
--   * 1 pointer per element to the actual data
-- * Rec requires (2n) words + size of Fields
--   * 1 word per (:&) constructor
--   * 1 word for the pointer to the element
module Data.Vinyl.ARec.Internal
  ( ARec (..)
  , ToARec
  , IndexableField
  , arec
  , ARecBuilder (..)
  , arcons
  , arnil
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
import Data.Vinyl.ARec.Internal.SmallArray
import Control.Monad.ST

import Unsafe.Coerce
#if __GLASGOW_HASKELL__ < 806
import Data.Constraint.Forall (Forall)
#endif
import Data.Type.Coercion     (Coercion (..))
import GHC.Types

-- | An array-backed extensible record with constant-time field
-- access.
newtype ARec (f :: k -> *) (ts :: [k]) = ARec SmallArray
type role ARec representational nominal

-- | Get the ith element from the ARec
unsafeIxARec
  :: forall a k (f :: k -> *) (ts :: [k]).
     ARec f ts
  -> Int
  -> a
unsafeIxARec (ARec ar) ix = indexSmallArray ar ix
{-# INLINE unsafeIxARec #-}

-- | Given that @xs@ and @ys@ have the same length, and mapping
-- @f@ over @xs@ and @g@ over @ys@ produces lists whose elements
-- are pairwise 'Coercible', @ARec f xs@ and @ARec g ys@ are
-- 'Coercible'.
arecRepsMatchCoercion :: AllRepsMatch f xs g ys => Coercion (ARec f xs) (ARec g ys)
arecRepsMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())

-- | Given that @forall x. Coercible (f x) (g x)@, produce a coercion from
-- @ARec f xs@ to @ARec g xs@. While the constraint looks a lot like
-- @Coercible f g@, it is actually weaker.

#if __GLASGOW_HASKELL__ >= 806
arecConsMatchCoercion ::
  (forall (x :: k). Coercible (f x) (g x)) => Coercion (ARec f xs) (ARec g xs)
arecConsMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())
#else
arecConsMatchCoercion :: forall k (f :: k -> *) (g :: k -> *) (xs :: [k]).
  Forall (Similar f g) => Coercion (Rec f xs) (Rec g xs)
-- Why do we need this? No idea, really. I guess some change in
-- newtype handling for Coercible in 8.6?
arecConsMatchCoercion = unsafeCoerce (Coercion :: Coercion (Rec f xs) (Rec f xs))
#endif

-- Using a class instead of a recursive function allows aRecValues to be
-- completely inlined
class ToARec (us :: [k]) where
  aRecValues :: Rec f us -> ARecBuilder f us

instance ToARec '[] where
  aRecValues RNil = arnil
  {-# INLINE aRecValues #-}

instance ToARec us => ToARec (u ': us) where
  aRecValues (x :& xs) = x `arcons` aRecValues xs
  {-# INLINE aRecValues #-}

-- | Convert a 'Rec' into an 'ARec' for constant-time field access.
toARec
  :: forall f ts.
     (NatToInt (RLength ts), ToARec ts)
  => Rec f ts
  -> ARec f ts
toARec rs = arec (aRecValues rs)
{-# INLINE toARec #-}

{-
-- This is sensible, but the ergonomics are likely quite bad thanks to the
-- interaction between Coercible resolution and resolution in the presence of
-- quantified constraints. Is there a good way to do this?

arecConsMatchCoercible :: forall k f g rep (r :: TYPE rep).
     (forall (x :: k). Coercible (f x) (g x))
  => ((forall (xs :: [k]). Coercible (ARec f xs) (ARec g xs)) => r) -> r
arecConsMatchCoercible f = f
-}

-- | An efficient builder for ARec values
--
-- Use the pseudo-constructors 'arcons' and 'arnil' to construct an
-- 'ARecBuilder' and then turn it into an 'ARec' with 'arec'
--
-- Example: (requires -XOverloadedLabels and )
--
-- > user :: ARec ElField '[ "name"   ::: String
-- >                       , "age"    ::: Int
-- >                       , "active" ::: Bool]
-- > user = arec (  #name   =: "Peter"
-- >             `arcons` #age    =: 4
-- >             `arcons` #active =: True
-- >             `arcons` arnil
-- >             )
newtype ARecBuilder f us =
  -- A function that writes values to the correct position in the underlying array
  -- Takes the current index
  ARecBuilder ( forall s.
                Int -- Index to write to
              -> SmallMutableArray s -- Arrray to write to
              -> ST s ()
              )

infixr 1 `arcons`
-- | Pseudo-constructor for an ARecBuilder
--
-- "Cons" a field to an ARec under construction
--
-- See 'ARecBuilder'
arcons :: f u -> ARecBuilder f us -> ARecBuilder f (u ': us)
arcons !v (ARecBuilder fvs) = ARecBuilder $ \i mArr -> do
    writeSmallArray mArr i v
    fvs (i+1) mArr
{-# INLINE arcons #-}

-- | Pseudo-constructor for 'ARecBuilder'
--
-- Build an ARec without fields
--
-- See 'ARecBuilder'
arnil :: ARecBuilder f '[]
arnil = ARecBuilder $ \_i _arr -> return ()
{-# INLINE arnil #-}

-- | Turn an ARecBuilder into an ARec
--
-- See 'ARecBuilder'
arec
  :: forall k (us :: [k] ) f
  . (NatToInt (RLength us)) =>
      ARecBuilder f us
  -> ARec f us
arec (ARecBuilder fillArray) = ARec $
  runST $ withNewSmallArray (natToInt @(RLength us))
          $ fillArray 0
{-# INLINE arec #-}

-- | Defines a constraint that lets us index into an 'ARec' in order
-- to produce a 'Rec' using 'fromARec'.
class (NatToInt (RIndex t ts)) => IndexableField ts t where
instance (NatToInt (RIndex t ts)) => IndexableField ts t where

-- | Convert an 'ARec' into a 'Rec'.
fromARec :: forall f ts.
            (RecApplicative ts, RPureConstrained (IndexableField ts) ts)
         => ARec f ts -> Rec f ts
fromARec ar = rpureConstrained @(IndexableField ts) aux
  where aux :: forall t. NatToInt (RIndex t ts) => f t
        aux = unsafeIxARec ar (natToInt @(RIndex t ts))
{-# INLINE fromARec #-}

-- | Get a field from an 'ARec'.
aget :: forall t f ts. (NatToInt (RIndex t ts)) => ARec f ts -> f t
aget ar = unsafeIxARec ar (natToInt @(RIndex t ts))
{-# INLINE aget #-}

-- | Set a field in an 'ARec'.
unsafeAput :: forall t t' f ts ts'. (NatToInt (RIndex t ts))
      => f t' -> ARec f ts -> ARec f ts'
unsafeAput x (ARec arr) = ARec $ runST $
  withThawedSmallArray arr $ \mArr ->
    writeSmallArray mArr (natToInt @(RIndex t ts)) x
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
arecGetSubset (ARec arr) =
  ARec $ runST $
    withNewSmallArray (natToInt @(RLength rs)) $ \mArr ->
      go mArr 0 (indexWitnesses @(RImage rs ss))
  where
    go :: SmallMutableArray s -> Int -> [Int] -> ST s ()
    go _mArr _to [] = return ()
    go mArr to (from : froms) = do
      writeSmallArray mArr to (indexSmallArray arr from :: Any)
      go mArr (to + 1) froms
{-# INLINE arecGetSubset #-}

-- | Set a subset of a larger record's fields to all of the fields of
-- a smaller record.
arecSetSubset :: forall rs ss f. (IndexWitnesses (RImage rs ss))
              => ARec f ss -> ARec f rs -> ARec f ss
arecSetSubset (ARec arrBig) (ARec arrSmall) = ARec $ runST $
  withThawedSmallArray arrBig $ \mArr -> do
    go mArr 0 (indexWitnesses @(RImage rs ss))
  where
    go :: SmallMutableArray s -> Int -> [Int] -> ST s ()
    go _mArr _ [] = return ()
    go mArr from (to : tos) = do
      writeSmallArray mArr to (indexSmallArray arrSmall from)
      go mArr (from + 1) tos
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
