{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

-- | Recursive definitions of various core vinyl functions. These are
-- simple definitions that put less strain on the compiler. They are
-- expected to have slower run times, but faster compile times than
-- the definitions in "Data.Vinyl.Core".
module Data.Vinyl.Recursive where

import Data.Kind
import Data.Proxy (Proxy(..))
import Data.Vinyl.Core (rpure, RecApplicative, Rec(..), Dict(..))
import Data.Vinyl.Functor (Compose(..), (:.), Lift(..), Const(..))
import Data.Vinyl.TypeLevel

-- | Two records may be pasted together.
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

-- | A shorthand for 'rappend'.
(<+>)
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
(<+>) = rappend

-- | 'Rec' @_ rs@ with labels in kind @u@ gives rise to a functor @Hask^u ->
-- Hask@; that is, a natural transformation between two interpretation functors
-- @f,g@ may be used to transport a value from 'Rec' @f rs@ to 'Rec' @g rs@.
rmap
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
rmap _ RNil = RNil
rmap η (x :& xs) = η x :& (η `rmap` xs)
{-# INLINE rmap #-}

-- | A shorthand for 'rmap'.
(<<$>>)
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
(<<$>>) = rmap
{-# INLINE (<<$>>) #-}

-- | An inverted shorthand for 'rmap'.
(<<&>>)
  :: Rec f rs
  -> (forall x. f x -> g x)
  -> Rec g rs
xs <<&>> f = rmap f xs
{-# INLINE (<<&>>) #-}

-- | A record of components @f r -> g r@ may be applied to a record of @f@ to
-- get a record of @g@.
rapply
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
rapply RNil RNil = RNil
rapply (f :& fs) (x :& xs) = getLift f x :& (fs `rapply` xs)
{-# INLINE rapply #-}

-- | A shorthand for 'rapply'.
(<<*>>)
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
(<<*>>) = rapply
{-# INLINE (<<*>>) #-}

-- | A record may be traversed with respect to its interpretation functor. This
-- can be used to yank (some or all) effects from the fields of the record to
-- the outside of the record.
rtraverse
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs
{-# INLINABLE rtraverse #-}

-- | Given a natural transformation from the product of @f@ and @g@ to @h@, we
-- have a natural transformation from the product of @'Rec' f@ and @'Rec' g@ to
-- @'Rec' h@. You can also think about this operation as zipping two records
-- with the same element types but different interpretations.
rzipWith
  :: (forall x  .     f x  ->     g x  ->     h x)
  -> (forall xs . Rec f xs -> Rec g xs -> Rec h xs)
rzipWith m = \r -> case r of
  RNil        -> \RNil        -> RNil
  (fa :& fas) -> \(ga :& gas) -> m fa ga :& rzipWith m fas gas

-- | Map each element of a record to a monoid and combine the results.
rfoldMap :: forall f m rs.
     Monoid m
  => (forall x. f x -> m)
  -> Rec f rs
  -> m
rfoldMap f = go mempty
  where
  go :: forall ss. m -> Rec f ss -> m
  go !m record = case record of
    RNil -> m
    r :& rs -> go (mappend m (f r)) rs
  {-# INLINABLE go #-}
{-# INLINE rfoldMap #-}

-- | A record with uniform fields may be turned into a list.
recordToList
  :: Rec (Const a) rs
  -> [a]
recordToList RNil = []
recordToList (x :& xs) = getConst x : recordToList xs


-- | Sometimes we may know something for /all/ fields of a record, but when
-- you expect to be able to /each/ of the fields, you are then out of luck.
-- Surely given @∀x:u.φ(x)@ we should be able to recover @x:u ⊢ φ(x)@! Sadly,
-- the constraint solver is not quite smart enough to realize this and we must
-- make it patently obvious by reifying the constraint pointwise with proof.
reifyConstraint
  :: RecAll f rs c
  => proxy c
  -> Rec f rs
  -> Rec (Dict c :. f) rs
reifyConstraint prx rec =
  case rec of
    RNil -> RNil
    (x :& xs) -> Compose (Dict x) :& reifyConstraint prx xs

-- | Build a record whose elements are derived solely from a
-- constraint satisfied by each.
rpureConstrained :: forall u c (f :: u -> Type) proxy ts.
                    (AllConstrained c ts, RecApplicative ts)
                 => proxy c -> (forall a. c a => f a) -> Rec f ts
rpureConstrained _ f = go (rpure Proxy)
  where go :: AllConstrained c ts' => Rec Proxy ts' -> Rec f ts'
        go RNil = RNil
        go (_ :& xs) = f :& go xs

-- | Build a record whose elements are derived solely from a
-- list of constraint constructors satisfied by each.
rpureConstraints :: forall cs (f :: Type -> Type) proxy ts. (AllAllSat cs ts, RecApplicative ts)
                 => proxy cs -> (forall a. AllSatisfied cs a => f a) -> Rec f ts
rpureConstraints _ f = go (rpure Nothing)
  where go :: AllAllSat cs ts' => Rec Maybe ts' -> Rec f ts'
        go RNil = RNil
        go (_ :& xs) = f :& go xs
