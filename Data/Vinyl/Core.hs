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
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
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
import Data.Coerce (Coercible)
#if __GLASGOW_HASKELL__ < 808
import Data.Monoid (Monoid)
#endif
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Functor.Product (Product(Pair))
import Data.List (intercalate)
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Type.Coercion (TestCoercion (..), Coercion (..))
import GHC.Generics
import GHC.Types (Constraint, Type)
import Unsafe.Coerce (unsafeCoerce)
import Control.DeepSeq (NFData, rnf)
#if __GLASGOW_HASKELL__ < 806
import Data.Constraint.Forall (Forall)
#endif

-- | A record is parameterized by a universe @u@, an interpretation @f@ and a
-- list of rows @rs@.  The labels or indices of the record are given by
-- inhabitants of the kind @u@; the type of values at any label @r :: u@ is
-- given by its interpretation @f r :: *@.
data Rec :: (u -> Type) -> [u] -> Type where
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

-- | Combine two records by combining their fields using the given
-- function. The first argument is a binary operation for combining
-- two values (e.g. '(<>)'), the second argument takes a record field
-- into the type equipped with the desired operation, the third
-- argument takes the combined value back to a result type.
rcombine :: (RMap rs, RApply rs)
         => (forall a. m a -> m a -> m a)
         -> (forall a. f a -> m a)
         -> (forall a. m a -> g a)
         -> Rec f rs
         -> Rec f rs
         -> Rec g rs
rcombine smash toM fromM x y =
  rmap fromM (rapply (rmap (Lift . smash) x') y')
  where x' = rmap toM x
        y' = rmap toM y

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

-- | While 'rtraverse' pulls the interpretation functor out of the
-- record, 'rtraverseIn' pushes the interpretation functor in to each
-- field type. This is particularly useful when you wish to discharge
-- that interpretation on a per-field basis. For instance, rather than
-- a @Rec IO '[a,b]@, you may wish to have a @Rec Identity '[IO a, IO
-- b]@ so that you can evaluate a single field to obtain a value of
-- type @Rec Identity '[a, IO b]@.
rtraverseIn :: forall h f g rs.
               (forall a. f a -> g (ApplyToField h a))
            -> Rec f rs
            -> Rec g (MapTyCon h rs)
rtraverseIn _ RNil = RNil
rtraverseIn f (x :& xs) = f x :& rtraverseIn f xs
{-# INLINABLE rtraverseIn #-}

-- | Push an outer layer of interpretation functor into each field.
rsequenceIn :: forall f g (rs :: [Type]). (Traversable f, Applicative g)
            => Rec (f :. g) rs -> Rec g (MapTyCon f rs)
rsequenceIn = rtraverseIn @f (sequenceA . getCompose)
{-# INLINABLE rsequenceIn #-}

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

-- | Capture a type class instance dictionary. See
-- 'Data.Vinyl.Lens.getDict' for a way to obtain a 'DictOnly' value
-- from an 'RPureConstrained' constraint.
data DictOnly (c :: k -> Constraint) a where
  DictOnly :: forall c a. c a => DictOnly c a

-- | A useful technique is to use 'rmap (Pair (DictOnly @MyClass))' on
-- a 'Rec' to pair each field with a type class dictionary for
-- @MyClass@. This helper can then be used to eliminate the original.
withPairedDict :: (c a => f a -> r) -> Product (DictOnly c) f a -> r
withPairedDict f (Pair DictOnly x) = f x

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
  mappend = (<>)

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  mappend = (<>)

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

instance ReifyConstraint NFData f xs => NFData (Rec f xs) where
  rnf = go . reifyConstraint @NFData
    where
      go :: forall elems. Rec (Dict NFData :. f) elems -> ()
      go RNil = ()
      go (Compose (Dict x) :& xs) = rnf x `seq` go xs

type family Head xs where
  Head (x ': _) = x
type family Tail xs where
  Tail (_ ': xs) = xs

type family AllRepsMatch_ (f :: j -> Type) (xs :: [j]) (g :: k -> Type) (ys :: [k]) :: Constraint where
  AllRepsMatch_ f (x ': xs) g ys =
    ( ys ~ (Head ys ': Tail ys)
    , Coercible (f x) (g (Head ys))
    , AllRepsMatch_ f xs g (Tail ys) )
  AllRepsMatch_ _ '[] _ ys = ys ~ '[]

-- | @AllRepsMatch f xs g ys@ means that @xs@ and @ys@ have the
-- same lengths, and that mapping @f@ over @xs@ and @g@ over @ys@
-- produces lists whose corresponding elements are 'Coercible' with
-- each other. For example, the following hold:
--
-- @AllRepsMatch Proxy '[1,2,3] Proxy '[4,5,6]@
-- @AllRepsMatch Sum '[Int,Word] Identity '[Min Int, Max Word]@
type AllRepsMatch f xs g ys = (AllRepsMatch_ f xs g ys, AllRepsMatch_ g ys f xs)

-- This two-sided approach means that the *length* of each list
-- can be inferred from the length of the other. I don't know how
-- useful that is in practice, but we get it almost for free.

-- | Given that for each element @x@ in the list @xs@,
repsMatchCoercion :: AllRepsMatch f xs g ys => Coercion (Rec f xs) (Rec g ys)
repsMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())

{-
-- "Proof" that repsMatchCoercion is sensible.
repsMatchConvert :: AllRepsMatch f xs g ys => Rec f xs -> Rec g ys
repsMatchConvert RNil = RNil
repsMatchConvert (x :& xs) = coerce x :& repsMatchConvert xs
-}

#if __GLASGOW_HASKELL__ >= 806
consMatchCoercion ::
  (forall (x :: k). Coercible (f x) (g x)) => Coercion (Rec f xs) (Rec g xs)
#else
consMatchCoercion :: forall k (f :: k -> Type) (g :: k -> Type) (xs :: [k]).
  Forall (Similar f g) => Coercion (Rec f xs) (Rec g xs)
#endif
consMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())
{-
-- "Proof" that consMatchCoercion is sensible.
consMatchConvert ::
  (forall (x :: k). Coercible (f x) (g x)) => Rec f xs -> Rec g xs
consMatchConvert RNil = RNil
consMatchConvert (x :& xs) = coerce x :& consMatchConvert xs

-- And for old GHC.
consMatchConvert' :: forall k (f :: k -> Type) (g :: k -> Type) (xs :: [k]).
  Forall (Similar f g) => Rec f xs -> Rec g xs
consMatchConvert' RNil = RNil
consMatchConvert' ((x :: f x) :& xs) =
  case inst :: Forall (Similar f g) DC.:- Similar f g x of
    DC.Sub DC.Dict -> coerce x :& consMatchConvert' xs
-}

{-
-- This is sensible, but I suspect the ergonomics will be awful
-- thanks to the interaction between Coercible constraint resolution
-- and constraint resolution with quantified constraints. Is there
-- a good way to accomplish it?

-- | Given
--
-- @
-- forall x. Coercible (f x) (g x)
-- @
--
-- provide the constraint
--
-- @
-- forall xs. Coercible (Rec f xs) (Rec g xs)
-- @
consMatchCoercible :: forall k f g rep (r :: TYPE rep).
     (forall (x :: k). Coercible (f x) (g x))
  => ((forall (xs :: [k]). Coercible (Rec f xs) (Rec g xs)) => r) -> r
consMatchCoercible f = case unsafeCoerce @(Zouch f f) @(Zouch f g) (Zouch $ \r -> r) of
  Zouch q -> q f

newtype Zouch (f :: k -> Type) (g :: k -> Type) =
  Zouch (forall rep (r :: TYPE rep). ((forall (xs :: [k]). Coercible (Rec f xs) (Rec g xs)) => r) -> r)
-}
