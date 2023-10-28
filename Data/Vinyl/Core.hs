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

{- |
A record is parameterized by a universe @u@, an interpretation @f@ and a
list of rows @rs@.  The labels or indices of the record are given by
inhabitants of the kind @u@; the type of values at any label @r :: u@ is
given by its interpretation @f r :: *@.

>>> :set -XDataKinds
>>> import Data.Vinyl.Functor (Identity(Identity))
>>> testRec = Identity 3 :& Identity "Hi" :& RNil
>>> :t testRec
testRec :: Num r => Rec Identity '[r, [Char]]
>>> testRec :: Rec Identity '[Int, String]
{3, "Hi"}

>>> testRec = Just 3 :& Nothing :& Just "Hi" :& RNil
>>> :t testRec
testRec :: Num r1 => Rec Maybe '[r1, r2, [Char]]

>>> :set -XTypeApplications
>>> import Data.Vinyl.Functor (ElField(Field))
>>> testRec = Field @'("name", String) "Alice" :& Field @'("age", Int) 20 :& RNil
>>> :t testRec
testRec :: Rec ElField '[ '("name", String), '("age", Int)]
>>> testRec
{name :-> "Alice", age :-> 20}
-}
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

{- |
Two records may be pasted together.

>>> :set -XScopedTypeVariables
>>> testRec1 :: Rec Maybe '[Int, String] = Just 3 :& Just "Hi" :& RNil
>>> testRec2 :: Rec Maybe '[Double, [Double]] = Nothing :& Just [3.0, 2.2] :& RNil
>>> appendedTestRec = rappend testRec1 testRec2
>>> :t appendedTestRec
appendedTestRec :: Rec Maybe '[Int, [Char], Double, [Double]]
-}
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

{- |
A shorthand for 'rappend'.

>>> :set -XScopedTypeVariables
>>> testRec1 :: Rec Maybe '[Int, String] = Just 3 :& Just "Hi" :& RNil
>>> testRec2 :: Rec Maybe '[Double, [Double]] = Nothing :& Just [3.0, 2.2] :& RNil
>>> appendedTestRecs = testRec1 <+> testRec2
>>> :t appendedTestRecs
appendedTestRecs :: Rec Maybe '[Int, [Char], Double, [Double]]
-}
(<+>)
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
(<+>) = rappend

{- | Combine two records by combining their fields using the given
function. The first argument is a binary operation for combining
two values (e.g. '(<>)'), the second argument takes a record field
into the type equipped with the desired operation, the third
argument takes the combined value back to a result type.

This function makes no assumption on the types stored in the record and is thus
limited to basic operations that can be executed on all types (as defined via
the unconstrained forall a in arguments 1,2 and 3). The
following snippet shows how to put each entry of a Rec Maybe in a list
(second argument), concatenating them (first argument), and then outputting a
Rec [] instead of a Rec Maybe—something which can be done for any type:

>>> import Data.Maybe (maybeToList)
>>> combineAsList = rcombine (<>) maybeToList id

>>> testRec1 :: Rec Maybe '[String, String] = Just "Ho" :& Just "Hi" :& RNil
>>> combineAsList testRec1 testRec1
{["Ho","Ho"], ["Hi","Hi"]}

We can't combine testRec1 with rcombine such that the Strings would be
concatenated directly because that would make assumptions on the types and
violate the unconstrained forall a. However, we can use combineAsList now on
any other Rec Maybe as well.

>>> testRec2 :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> combineAsList testRec2 testRec2
{["Ho","Ho"], [3.0,3.0], []}

-}

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

{- |
'Rec' @_ rs@ with labels in kind @u@ gives rise to a functor @Hask^u ->
Hask@; that is, a natural transformation between two interpretation functors
@f,g@ may be used to transport a value from 'Rec' @f rs@ to 'Rec' @g rs@.

Here is an example:

>>> import Data.Maybe (maybeToList)
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> rmap maybeToList testRec
{["Ho"], [3.0], []}

Similar to other functions in this module, we can not use rmap to map type
specific functions over a record. Only functions that work for any type can be
used.
-}
class RMap rs where
  rmap :: (forall x. f x -> g x) -> Rec f rs -> Rec g rs

instance RMap '[] where
  rmap _ RNil = RNil
  {-# INLINE rmap #-}

instance RMap xs => RMap (x ': xs) where
  rmap f (x :& xs) = f x :& rmap f xs
  {-# INLINE rmap #-}

{- | A shorthand for 'rmap'.
>>> import Data.Maybe (maybeToList)
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> maybeToList <<$>> testRec
{["Ho"], [3.0], []}
-}
(<<$>>)
  :: RMap rs
  => (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
(<<$>>) = rmap
{-# INLINE (<<$>>) #-}

{- | An inverted shorthand for 'rmap'.
>>> import Data.Maybe (maybeToList)
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> testRec <<&>> maybeToList 
{["Ho"], [3.0], []}
-}
(<<&>>)
  :: RMap rs
  => Rec f rs
  -> (forall x. f x -> g x)
  -> Rec g rs
xs <<&>> f = rmap f xs
{-# INLINE (<<&>>) #-}

{- |
A record of components @f r -> g r@ may be applied to a record of @f@ to
get a record of @g@.

>>> import Data.Vinyl.Functor (Const(Const), Lift(Lift))
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> :{
funcRec = Lift (\x -> Const "String")
  :& Lift (\x -> Const "Double")
  :& Lift (\x -> Const "Int")
  :& RNil
:}

>>> recordToList $ rapply funcRec testRec
["String","Double","Int"]
-}
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

{- |
A shorthand for 'rapply'.

>>> import Data.Vinyl.Functor (Const(Const), Lift(Lift))
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>> :{
funcRec = Lift (\x -> Const "String")
  :& Lift (\x -> Const "Double")
  :& Lift (\x -> Const "Int")
  :& RNil
:}

>>> recordToList $ funcRec <<*>> testRec
["String","Double","Int"]
-}
(<<*>>)
  :: RApply rs
  => Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
(<<*>>) = rapply
{-# INLINE (<<*>>) #-}

{- |
Given a section of some functor, records in that functor of any size are
inhabited. Note that you need a value that can inhabit any type in the record.
It is not possible, with this function, to derive a default with a type class
method. Here are a few examples:

>>> testRec :: Rec Maybe '[Double, String] = rpure Nothing
>>> testRec
{Nothing, Nothing}
>>> testRec :: Rec [] '[Double, String] = rpure []
>>> testRec
{[], []}
>>> import Data.Proxy (Proxy(Proxy))
>>> testRec :: Rec Proxy '[Double, String] = rpure Proxy
>>> testRec
{Proxy, Proxy}
-}
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

{- |
A record may be traversed with respect to its interpretation functor. This
can be used to yank (some or all) effects from the fields of the record to
the outside of the record.

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>>
:{
ext :: Maybe x -> Maybe (Identity x)
ext (Just x) = Just (Identity x)
ext Nothing = Nothing
:}
>>> rtraverse ext testRec
Nothing

Here is another interesting example that allows reading a record
interactively.

>>> :set -XTypeOperators
>>> import Text.Read (readMaybe)
>>> import Data.Vinyl.Functor ((:.), Compose(Compose), getCompose)
>>>
:{
readMaybeIO :: forall a. Read a => (IO :. Maybe) a
readMaybeIO = Compose $ readMaybe <$> getLine
testRec :: Rec (IO :. Maybe) [String, Double, Int]
testRec = rpureConstrained @Read readMaybeIO
:}
>>> :t rtraverse getCompose testRec
rtraverse getCompose testRec
  :: IO (Rec Maybe '[String, Double, Int])

And one more with State

>>> :set -package mtl
>>> import Control.Monad.State (StateT(StateT), runState, State, state)
>>> import qualified Data.Vinyl.Functor as V
>>> import qualified Data.Functor.Identity as I
>>> testRec1 :: Rec (State Int) '[Int, Double] = state (\s -> (s, 2 * s)) :& state (\s -> (fromIntegral s, 3 * s)) :& RNil
>>>
:{
ext :: State Int a -> State Int (V.Identity a)
ext (StateT f) = StateT $ \s -> let (a, s') = I.runIdentity (f s) in I.Identity (V.Identity a, s')
:}
>>> stateRec = rtraverse ext testRec1
>>> runState stateRec 1
({1, 2.0},6)
>>> runState (sequence $ replicate 5 stateRec) 1
([{1, 2.0},{6, 12.0},{36, 72.0},{216, 432.0},{1296, 2592.0}],7776)
-}
rtraverse
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs
{-# INLINABLE rtraverse #-}

{- |
While 'rtraverse' pulls the interpretation functor out of the
record, 'rtraverseIn' pushes the interpretation functor in to each
field type. This is particularly useful when you wish to discharge
that interpretation on a per-field basis. For instance, rather than
a @Rec IO '[a,b]@, you may wish to have a @Rec Identity '[IO a, IO
b]@ so that you can evaluate a single field to obtain a value of
type @Rec Identity '[a, IO b]@.

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> testRec :: Rec Maybe '[String, Double, Int] = Just "Ho" :& Just 3.0 :& Nothing :& RNil
>>>
:{
push :: forall x. Maybe x -> Identity (Maybe x)
push (Just x) = Identity (Just x)
push Nothing = Identity Nothing
:}
>>> :t rtraverseIn push testRec
rtraverseIn push testRec
  :: Rec Identity '[Maybe [Char], Maybe Double, Maybe Int]
-}
rtraverseIn :: forall h f g rs.
               (forall a. f a -> g (ApplyToField h a))
            -> Rec f rs
            -> Rec g (MapTyCon h rs)
rtraverseIn _ RNil = RNil
rtraverseIn f (x :& xs) = f x :& rtraverseIn f xs
{-# INLINABLE rtraverseIn #-}

{- |
Push an outer layer of interpretation functor into each field.

>>> :set -XTypeOperators
>>> import Data.Vinyl.Functor ((:.), Identity(Identity), Compose(Compose))
:{
testRec :: Rec (Maybe :. Identity) '[ String, Double, Int] =
    Compose (Just (Identity "Ho"))
    :& Compose (Just (Identity 3.0))
    :& Compose Nothing
    :& RNil
:}
>>> :t rsequenceIn testRec
rsequenceIn testRec
  :: Rec Identity '[Maybe [Char], Maybe Double, Maybe Int]

Note that this function can't be applied as easily to anything
composed with interpretation functors as inner layer that take custom
kinds as inputs. For example, when dealing with (Maybe :. ElField),
Maybe the function can't be applied because that would require applying Maybe
to a (Symbol, *) kind.
-}
rsequenceIn :: forall f g (rs :: [Type]). (Traversable f, Applicative g)
            => Rec (f :. g) rs -> Rec g (MapTyCon f rs)
rsequenceIn = rtraverseIn @f (sequenceA . getCompose)
{-# INLINABLE rsequenceIn #-}

{- |
Given a natural transformation from the product of @f@ and @g@ to @h@, we
have a natural transformation from the product of @'Rec' f@ and @'Rec' g@ to
@'Rec' h@. You can also think about this operation as zipping two records
with the same element types but different interpretations.

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> testRec1 :: Rec Identity '[String, Double] = Identity "Joe" :& Identity 20.0 :& RNil
>>> testRec2 :: Rec [] '[String, Double] = ["John"] :& [15.3] :& RNil
>>> rzipWith (\(Identity a) xs -> a:xs) testRec1 testRec2
{["Joe","John"], [20.0,15.3]}
-}
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

{- |
This function allows to collect all elements of a record in a monoid. The
collector function can be specialized for a particular interpretation functor
but has to be applicable for any type. It's therefore most useful to collect
effects.

>>> testRec1 :: Rec Maybe '[String, Double] = Just "Anna" :& Nothing :& RNil
>>>
:{
func :: forall x. Maybe x -> String
func (Just x) = "Just "
func Nothing = "Nothing "
:}
>>> rfoldMap func testRec1
"Just Nothing "
-}
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

{- |
Wrap up a value with a capability given by its type. In other words, the
existance of a value x :: Dict c a proves the existence of an instance c a.
This is useful to help the type checker realize that a value in a record does
indeed have a certain instance.

To understand better why this type is useful, consider the following function
that doesn't typecheck:

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> import Data.Vinyl.TypeLevel (AllConstrained)
>>>
:{
func :: forall rs. (RMap rs, AllConstrained Num rs) => Rec Identity rs -> Rec Identity rs
func = rmap (\(Identity x) -> Identity (x+1))
:}
...
    • Could not deduce (Num x) arising from a use of ‘+’
      from the context: (RMap rs, AllConstrained Num rs)
        bound by the type signature for:
                   func :: forall (rs :: [*]).
                           (RMap rs, AllConstrained Num rs) =>
                           Rec Identity rs -> Rec Identity rs
...

Dict allows to encapsulate a constraint directly in the type such that anything
wrapped in it automatically fullfils it. RMap understands that as well:

>>> import Data.Vinyl.Functor (Identity(Identity))
>>>
:{
func :: forall rs. RMap rs => Rec (Dict Num) rs -> Rec Identity rs
func = rmap (\(Dict x) -> Identity (x + 1))
:}
>>> testRec :: Rec (Dict Num) '[Double, Int] = Dict 1.0 :& Dict 0 :& RNil
>>> func testRec
{2.0, 1}
-}
data Dict c a where
  Dict
    :: c a
    => a
    -> Dict c a

{- |
Sometimes we may know something for /all/ fields of a record, but when
you expect to be able to /each/ of the fields, you are then out of luck.
Surely given @∀x:u.φ(x)@ we should be able to recover @x:u ⊢ φ(x)@! Sadly,
the constraint solver is not quite smart enough to realize this and we must
make it patently obvious by reifying the constraint pointwise with proof.

Here is an example how this can be used in practise:

>>> import Data.Vinyl.Functor (ElField(Field), Compose(Compose))
>>> testRec :: Rec ElField '[ '("age", Double), '("number", Int)] = Field 1.0 :& Field 0 :& RNil
>>> testRecWithConstraints = reifyConstraint @Num testRec
>>> rmap (\(Compose (Dict x)) -> x+1) testRecWithConstraints
{age :-> 2.0, number :-> 1}
-}
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

{- |
Build a record whose elements are derived solely from a
constraint satisfied by each.

>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XUndecidableInstances
>>> import Data.Vinyl.Derived (KnownField)
>>> import Data.Vinyl.TypeLevel (Snd)
>>> import Data.Vinyl.Functor (ElField(Field))
>>> import Data.Proxy (Proxy(Proxy))
>>> class (KnownField a, Monoid (Snd a)) => Helper a
>>> instance (KnownField a, Monoid (Snd a)) => Helper a
>>>
:{
testRec :: Rec ElField '[ '("list", [Double]), '("string", String) ]
testRec = rpureConstrained @Helper (Field mempty)
:}
>>> testRec
{list :-> [], string :-> ""}
-}
class RPureConstrained c ts where
  rpureConstrained :: (forall a. c a => f a) -> Rec f ts

instance RPureConstrained c '[] where
  rpureConstrained _ = RNil
  {-# INLINE rpureConstrained #-}

instance (c x, RPureConstrained c xs) => RPureConstrained c (x ': xs) where
  rpureConstrained f = f :& rpureConstrained @c @xs f
  {-# INLINE rpureConstrained #-}

{- |
Capture a type class instance dictionary. This data type can
be used together with rpureConstrained:

>>> import Data.Vinyl.Lens (rget)
>>>
:{
testRec :: Rec (DictOnly Num) '[Double, Int]
testRec = rpureConstrained @Num @'[Double, Int] DictOnly
:}
>>>
:{
val :: DictOnly Num Double
val = rget @Double testRec
:}

In itself not much can be done with a @DictOnly@. However,
it becomes useful together with the @Product@ Functor that allows
to pair it on the side with another value. That way we can keep
track of a constraint and use the other value for computations:

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> import Data.Functor.Product (Product(Pair))
>>>
:{
builder :: Num a => Product (DictOnly Num) Identity a
builder = Pair DictOnly (Identity 0)
:}
>>>
:{
testRec :: Rec (Product (DictOnly Num) Identity) '[Double, Int]
testRec = rpureConstrained @Num @'[Double, Int] builder
:}
>>> Pair DictOnly val = rget @Double testRec
>>> val
0.0
-}
data DictOnly (c :: k -> Constraint) a where
  DictOnly :: forall c a. c a => DictOnly c a

{- |
A useful technique is to use 'rmap (Pair (DictOnly @MyClass))' on a 'Rec' to
pair each field with a type class dictionary for @MyClass@. This helper can
then be used to apply a function to a DictOnly that is paired with another
field:

>>> import Data.Vinyl.Functor (Identity(Identity))
>>> import Data.Functor.Product (Product(Pair))
>>>
:{
testRec :: Rec (Product (DictOnly Num) Identity) '[Double, Int]
testRec = rpureConstrained @Num @'[Double, Int] (Pair DictOnly (Identity 0))
:}
>>> rmap (withPairedDict (fmap (2 +))) testRec
{2.0, 2}
-}
withPairedDict :: (c a => f a -> r) -> Product (DictOnly c) f a -> r
withPairedDict f (Pair DictOnly x) = f x

{- |
Build a record whose elements are derived solely from a
list of constraint constructors satisfied by each.

>>> import Data.Functor.Identity (Identity(Identity))
>>> rpureConstraints @'[Num, Enum] (succ (Identity 0)) :: Rec Identity '[Int, Double] 
{Identity 1, Identity 1.0}
-}
class RPureConstraints cs ts where
  rpureConstraints :: (forall a. AllSatisfied cs a => f a) -> Rec f ts

instance RPureConstraints cs '[] where
  rpureConstraints _ = RNil
  {-# INLINE rpureConstraints #-}

instance (AllSatisfied cs t, RPureConstraints cs ts)
  => RPureConstraints cs (t ': ts) where
  rpureConstraints f = f :& rpureConstraints @cs @ts f
  {-# INLINE rpureConstraints #-}

{- |
Records may be shown insofar as their points may be shown.
'reifyConstraint' is used to great effect here.

>>> "record with Nothings: " ++ (show (rpure Nothing :: Rec Maybe '[Int, Double]))
"record with Nothings: {Nothing, Nothing}"
-}
instance (RMap rs, ReifyConstraint Show f rs, RecordToList rs)
  => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . intercalate ", "
      . recordToList
      . rmap (\(Compose (Dict x)) -> Const $ show x)
      $ reifyConstraint @Show xs

{- |
The Semigroup instance is mapped to each element of the record.

>>> t1 = ["Alice", "Bob"] :& [20, 22] :& RNil
>>> t2 = ["Charlie", "Dan"] :& [24, 23] :& RNil
>>> t1 <> t2
{["Alice","Bob","Charlie","Dan"], [20,22,24,23]}
-}
instance Semigroup (Rec f '[]) where
  RNil <> RNil = RNil

instance (Semigroup (f r), Semigroup (Rec f rs))
  => Semigroup (Rec f (r ': rs)) where
  (x :& xs) <> (y :& ys) = (x <> y) :& (xs <> ys)

{- |
The Monoid instance is also mapped to each element of the record.

>>> mempty :: Rec [] '[Double, String, Int]
{[], [], []}
-}
instance Monoid (Rec f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = (mappend x y) :& (mappend xs ys)

{- |
The Eq instance is also just an element-wise comparison.

>>> import Data.Vinyl.Functor (ElField(Field))
>>> r1 :: Rec ElField '[ '("name", String), '("age", Int)] = Field "Alice":& Field 30 :& RNil
>>> r2 :: Rec ElField '[ '("name", String), '("age", Int)] = Field "Bob":& Field 25 :& RNil
>>> r1 == r2
False
-}
instance Eq (Rec f '[]) where
  _ == _ = True
instance (Eq (f r), Eq (Rec f rs)) => Eq (Rec f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

{- |
The Ord instance is element-wise, lexicographical ordering using the Monoid
instance of the Ordering type.

>>> import Data.Vinyl.Functor (ElField(Field))
>>> r1 :: Rec ElField '[ '("name", String), '("age", Int)] = Field "Alice":& Field 30 :& RNil
>>> r2 :: Rec ElField '[ '("name", String), '("age", Int)] = Field "Bob":& Field 25 :& RNil
>>> r1 < r2
True
-}
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

{- |
Analogous to 'Data.List.head'. Unlike the version in @singletons@,
this family merely gets stuck instead of producing a type error if
the list is empty. This is often better, because the type error that
would be produced here would be much less informative than one that
would likely be available where it's used.

>>> :k! Head '[Int, Double, String]
Head '[Int, Double, String] :: *
= Int
-}
type family Head xs where
  Head (x ': _) = x
  
{- |
Analogous to 'Data.List.tail'. Unlike the version in @singletons@,
this family merely gets stuck instead of producing a type error if
the list is empty. This is often better, because the type error that
would be produced here would be much less informative than one that
would likely be available where it's used.

>>> :k! Tail '[Int, Double, String]
Tail '[Int, Double, String] :: [*]
= '[Double, String]
-}
type family Tail xs where
  Tail (_ ': xs) = xs

type family AllRepsMatch_ (f :: j -> *) (xs :: [j]) (g :: k -> *) (ys :: [k]) :: Constraint where
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
consMatchCoercion :: forall k (f :: k -> *) (g :: k -> *) (xs :: [k]).
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
consMatchConvert' :: forall k (f :: k -> *) (g :: k -> *) (xs :: [k]).
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

newtype Zouch (f :: k -> *) (g :: k -> *) =
  Zouch (forall rep (r :: TYPE rep). ((forall (xs :: [k]). Coercible (Rec f xs) (Rec g xs)) => r) -> r)
-}
