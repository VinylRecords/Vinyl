{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Core vinyl definitions. The 'Rec' data type is defined here, but
-- also of interest are definitions commonly used functions like
-- 'rmap', 'rapply', and 'rtraverse'.
--
-- The definitions in this module are written in terms of type classes
-- so that the definitions may be specialized to each record type at
-- which they are used. This usually helps with runtime performance,
-- but can slow down compilation time. If you are experiencing poor
-- compile times, you may wish to try the semantically equivalent
-- definitions in the "Data.Vinyl.Recursive" module: they should
-- produce the same results given the same inputs as functions defined
-- in this module, but they will not be specialized to your record
-- type. Instead, they treat the record as a list of fields, so will
-- have performance linear in the size of the record.
module Data.Vinyl.Core where

import Data.Monoid (Monoid)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Functor
import Data.List (intercalate)
import Data.Vinyl.TypeLevel
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Type.Coercion (TestCoercion (..), Coercion (..))
import GHC.Generics

-- | A record is parameterized by a universe @u@, an interpretation @f@ and a
-- list of rows @rs@.  The labels or indices of the record are given by
-- inhabitants of the kind @u@; the type of values at any label @r :: u@ is
-- given by its interpretation @f r :: *@.
data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

infixr 7 :&
infixr 5  <+>
infixl 8 <<$>>
infixl 8 <<*>>

instance TestEquality f => TestEquality (Rec f) where
  testEquality RNil RNil = Just Refl
  testEquality (x :& xs) (y :& ys) = do
    Refl <- testEquality x y
    Refl <- testEquality xs ys
    Just Refl
  testEquality _ _ = Nothing

instance TestCoercion f => TestCoercion (Rec f) where
  testCoercion RNil RNil = Just Coercion
  testCoercion (x :& xs) (y :& ys) = do
    Coercion <- testCoercion x y
    Coercion <- testCoercion xs ys
    Just Coercion
  testCoercion _ _ = Nothing

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
class RMap rs where
  rmap :: (forall x. f x -> g x) -> Rec f rs -> Rec g rs

instance RMap '[] where
  rmap _ RNil = RNil
  {-# INLINE rmap #-}

instance RMap xs => RMap (x ': xs) where
  rmap f (x :& xs) = f x :& rmap f xs
  {-# INLINE rmap #-}

-- | A shorthand for 'rmap'.
(<<$>>)
  :: RMap rs
  => (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
(<<$>>) = rmap
{-# INLINE (<<$>>) #-}

-- | An inverted shorthand for 'rmap'.
(<<&>>)
  :: RMap rs
  => Rec f rs
  -> (forall x. f x -> g x)
  -> Rec g rs
xs <<&>> f = rmap f xs
{-# INLINE (<<&>>) #-}

-- | A record of components @f r -> g r@ may be applied to a record of @f@ to
-- get a record of @g@.
class RApply rs where
  rapply :: Rec (Lift (->) f g) rs
         -> Rec f rs
         -> Rec g rs

instance RApply '[] where
  rapply _ RNil = RNil
  {-# INLINE rapply #-}

instance RApply xs => RApply (x ': xs) where
  rapply (f :& fs) (x :& xs) = getLift f x :& (fs `rapply` xs)
  {-# INLINE rapply #-}

-- | A shorthand for 'rapply'.
(<<*>>)
  :: RApply rs
  => Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
(<<*>>) = rapply
{-# INLINE (<<*>>) #-}

-- | Given a section of some functor, records in that functor of any size are
-- inhabited.
class RecApplicative rs where
  rpure
    :: (forall x. f x)
    -> Rec f rs
instance RecApplicative '[] where
  rpure _ = RNil
  {-# INLINE rpure #-}
instance RecApplicative rs => RecApplicative (r ': rs) where
  rpure s = s :& rpure s
  {-# INLINE rpure #-}

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
rzipWith :: (RMap xs, RApply xs)
         => (forall x. f x -> g x -> h x) -> Rec f xs -> Rec g xs -> Rec h xs
rzipWith f = rapply . rmap (Lift . f)

-- | Map each element of a record to a monoid and combine the results.
class RFoldMap rs where
  rfoldMapAux :: Monoid m
              => (forall x. f x -> m)
              -> m
              -> Rec f rs
              -> m

instance RFoldMap '[] where
  rfoldMapAux _ m RNil = m
  {-# INLINE rfoldMapAux #-}

instance RFoldMap xs => RFoldMap (x ': xs) where
  rfoldMapAux f m (r :& rs) = rfoldMapAux f (mappend m (f r)) rs
  {-# INLINE rfoldMapAux #-}

rfoldMap :: forall rs m f. (Monoid m, RFoldMap rs)
         => (forall x. f x -> m) -> Rec f rs -> m
rfoldMap f = rfoldMapAux f mempty
{-# INLINE rfoldMap #-}

-- | A record with uniform fields may be turned into a list.
class RecordToList rs where
  recordToList :: Rec (Const a) rs -> [a]

instance RecordToList '[] where
  recordToList RNil = []
  {-# INLINE recordToList #-}

instance RecordToList xs => RecordToList (x ': xs) where
  recordToList (x :& xs) = getConst x : recordToList xs
  {-# INLINE recordToList #-}

-- | Wrap up a value with a capability given by its type
data Dict c a where
  Dict
    :: c a
    => a
    -> Dict c a

-- | Sometimes we may know something for /all/ fields of a record, but when
-- you expect to be able to /each/ of the fields, you are then out of luck.
-- Surely given @∀x:u.φ(x)@ we should be able to recover @x:u ⊢ φ(x)@! Sadly,
-- the constraint solver is not quite smart enough to realize this and we must
-- make it patently obvious by reifying the constraint pointwise with proof.
class ReifyConstraint c f rs where
  reifyConstraint
    :: Rec f rs
    -> Rec (Dict c :. f) rs

instance ReifyConstraint c f '[] where
  reifyConstraint RNil = RNil
  {-# INLINE reifyConstraint #-}

instance (c (f x), ReifyConstraint c f xs)
  => ReifyConstraint c f (x ': xs) where
  reifyConstraint (x :& xs) = Compose (Dict x) :& reifyConstraint xs
  {-# INLINE reifyConstraint #-}

-- | Build a record whose elements are derived solely from a
-- constraint satisfied by each.
class RPureConstrained c ts where
  rpureConstrained :: (forall a. c a => f a) -> Rec f ts

instance RPureConstrained c '[] where
  rpureConstrained _ = RNil
  {-# INLINE rpureConstrained #-}

instance (c x, RPureConstrained c xs) => RPureConstrained c (x ': xs) where
  rpureConstrained f = f :& rpureConstrained @c @xs f
  {-# INLINE rpureConstrained #-}

-- | Build a record whose elements are derived solely from a
-- list of constraint constructors satisfied by each.
class RPureConstraints cs ts where
  rpureConstraints :: (forall a. AllSatisfied cs a => f a) -> Rec f ts

instance RPureConstraints cs '[] where
  rpureConstraints _ = RNil
  {-# INLINE rpureConstraints #-}

instance (AllSatisfied cs t, RPureConstraints cs ts)
  => RPureConstraints cs (t ': ts) where
  rpureConstraints f = f :& rpureConstraints @cs @ts f
  {-# INLINE rpureConstraints #-}

-- | Records may be shown insofar as their points may be shown.
-- 'reifyConstraint' is used to great effect here.
instance (RMap rs, ReifyConstraint Show f rs, RecordToList rs)
  => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . intercalate ", "
      . recordToList
      . rmap (\(Compose (Dict x)) -> Const $ show x)
      $ reifyConstraint @Show xs

instance Semigroup (Rec f '[]) where
  RNil <> RNil = RNil

instance (Semigroup (f r), Semigroup (Rec f rs))
  => Semigroup (Rec f (r ': rs)) where
  (x :& xs) <> (y :& ys) = (x <> y) :& (xs <> ys)

instance Monoid (Rec f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = (mappend x y) :& (mappend xs ys)

instance Eq (Rec f '[]) where
  _ == _ = True
instance (Eq (f r), Eq (Rec f rs)) => Eq (Rec f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

instance Ord (Rec f '[]) where
  compare _ _ = EQ
instance (Ord (f r), Ord (Rec f rs)) => Ord (Rec f (r ': rs)) where
  compare (x :& xs) (y :& ys) = mappend (compare x y) (compare xs ys)

instance Storable (Rec f '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (f r), Storable (Rec f rs))
  => Storable (Rec f (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: f r) + sizeOf (undefined :: Rec f rs)
  {-# INLINE sizeOf #-}
  alignment _ =  alignment (undefined :: f r)
  {-# INLINE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: f r))
                return $ x :& xs
  {-# INLINE peek #-}
  poke ptr (!x :& xs) = poke (castPtr ptr) x >> poke (ptr `plusPtr` sizeOf (undefined :: f r)) xs
  {-# INLINE poke #-}

instance Generic (Rec f '[]) where
  type Rep (Rec f '[]) =
    C1 ('MetaCons "RNil" 'PrefixI 'False)
       (S1 ('MetaSel 'Nothing
          'NoSourceUnpackedness
          'NoSourceStrictness
          'DecidedLazy) U1)
  from RNil = M1 (M1 U1)
  to (M1 (M1 U1)) = RNil

instance (Generic (Rec f rs)) => Generic (Rec f (r ': rs)) where
  type Rep (Rec f (r ': rs)) =
    C1 ('MetaCons ":&" ('InfixI 'RightAssociative 7) 'False)
    (S1 ('MetaSel 'Nothing
         'NoSourceUnpackedness
         'SourceStrict
         'DecidedStrict)
       (Rec0 (f r))
      :*:
      S1 ('MetaSel 'Nothing
           'NoSourceUnpackedness
           'NoSourceStrictness
           'DecidedLazy)
         (Rep (Rec f rs)))
  from (x :& xs) = M1 (M1 (K1 x) :*: M1 (from xs))
  to (M1 (M1 (K1 x) :*: M1 xs)) = x :& to xs
