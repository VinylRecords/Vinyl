{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}

{-| This module uses 'RecAll' to extend common typeclass methods to records.
    Generally, it is preferable to use the original typeclass methods to these
    variants. For example, in most places where 'recCompare' could be used,
    you could use 'compare' instead. They are useful in scenarios
    that involve working on unknown subsets of a record's fields
    because 'RecAll' constraints can easily be weakened. An example of this
    is given at the bottom of this page.
-}

module Data.Vinyl.Class.Method
  ( -- * Mapping methods over records
    RecMapMethod(..)
  , rmapMethodF
  , mapFields
  , RecPointed(..)
    -- * Support for 'RecMapMethod'
  , FieldTyper, ApplyFieldTyper, PayloadType
    -- * Eq Functions
  ,  recEq
     -- * Ord Functions
  , recCompare
    -- * Monoid Functions
  , recMempty
  , recMappend
  , recMconcat
    -- * Num Functions
  , recAdd
  , recSubtract
  , recMultiply
  , recAbs
  , recSignum
  , recNegate
    -- * Bounded Functions
  , recMinBound
  , recMaxBound
    -- * Example
    -- $example
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Functor ((:.), ElField(..))
import Data.Vinyl.TypeLevel
import Data.Monoid

recEq :: RecAll f rs Eq => Rec f rs -> Rec f rs -> Bool
recEq RNil RNil = True
recEq (a :& as) (b :& bs) = a == b && recEq as bs

recCompare :: RecAll f rs Ord => Rec f rs -> Rec f rs -> Ordering
recCompare RNil RNil = EQ
recCompare (a :& as) (b :& bs) = compare a b <> recCompare as bs

-- | This function differs from the original 'mempty' in that
--   it takes an argument. In some cases, you will already
--   have a record of the type you are interested in, and
--   that can be passed an the argument. In other situations
--   where this is not the case, you may need the
--   interpretation function of the argument record to be
--   @Const ()@ or @Proxy@ so the you can generate the
--   argument with 'rpure'.
recMempty :: RecAll f rs Monoid => Rec proxy rs -> Rec f rs
recMempty RNil = RNil
recMempty (_ :& rs) = mempty :& recMempty rs

recMappend :: RecAll f rs Monoid => Rec f rs -> Rec f rs -> Rec f rs
recMappend RNil RNil = RNil
recMappend (a :& as) (b :& bs) = mappend a b :& recMappend as bs

-- | This function differs from the original 'mconcat'.
--   See 'recMempty'.
recMconcat :: RecAll f rs Monoid => Rec proxy rs -> [Rec f rs] -> Rec f rs
recMconcat p [] = recMempty p
recMconcat p (rec : recs) = recMappend rec (recMconcat p recs)

recAdd :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recAdd RNil RNil = RNil
recAdd (a :& as) (b :& bs) = (a + b) :& recAdd as bs

recSubtract :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recSubtract RNil RNil = RNil
recSubtract (a :& as) (b :& bs) = (a - b) :& recSubtract as bs

recMultiply :: RecAll f rs Num => Rec f rs -> Rec f rs -> Rec f rs
recMultiply RNil RNil = RNil
recMultiply (a :& as) (b :& bs) = (a * b) :& recSubtract as bs

recAbs :: RecAll f rs Num => Rec f rs -> Rec f rs
recAbs RNil = RNil
recAbs (a :& as) = abs a :& recAbs as

recSignum :: RecAll f rs Num => Rec f rs -> Rec f rs
recSignum RNil = RNil
recSignum (a :& as) = signum a :& recAbs as

recNegate :: RecAll f rs Num => Rec f rs -> Rec f rs
recNegate RNil = RNil
recNegate (a :& as) = negate a :& recAbs as

-- | This function differs from the original 'minBound'.
--   See 'recMempty'.
recMinBound :: RecAll f rs Bounded => Rec proxy rs -> Rec f rs
recMinBound RNil = RNil
recMinBound (_ :& rs) = minBound :& recMinBound rs

-- | This function differs from the original 'maxBound'.
--   See 'recMempty'.
recMaxBound :: RecAll f rs Bounded => Rec proxy rs -> Rec f rs
recMaxBound RNil = RNil
recMaxBound (_ :& rs) = maxBound :& recMaxBound rs

-- | When we wish to apply a typeclass method to each field of a
-- 'Rec', we typically care about typeclass instances of the record
-- field types irrespective of the record's functor context. To expose
-- the field types themselves, we utilize a constraint built from a
-- defunctionalized type family in the 'rmapMethod' method. The
-- symbols of the function space are defined by this data type.
data FieldTyper = FieldId | FieldSnd

-- | The interpretation function of the 'FieldTyper' symbols.
type family ApplyFieldTyper (f :: FieldTyper) (a :: k) :: * where
  ApplyFieldTyper 'FieldId a = a
  ApplyFieldTyper 'FieldSnd '(s, b) = b

-- | A mapping of record contexts into the 'FieldTyper' function
-- space. We explicitly match on 'ElField' to pick out the payload
-- type, and 'Compose' to pick out the inner-most context. All other
-- type constructor contexts are understood to not perform any
-- computation on their arguments.
type family FieldPayload (f :: u -> *) :: FieldTyper where
  FieldPayload ElField = 'FieldSnd
  FieldPayload (f :. g) = FieldPayload g
  FieldPayload f = 'FieldId

-- | Shorthand for combining 'ApplyFieldTyper' and 'FieldPayload'.
type family PayloadType f (a :: u) :: * where
  PayloadType f a = ApplyFieldTyper (FieldPayload f) a

-- | Generate a record from fields derived from type class
-- instances.
class RecPointed c (f :: u -> *) (ts :: [u]) where
  rpointMethod :: (forall (a :: u). c (f a) => f a) -> Rec f ts

instance RecPointed c f '[] where
  rpointMethod _ = RNil
  {-# INLINE rpointMethod #-}

instance (c (f t), RecPointed c f ts)
  => RecPointed c f (t ': ts) where
  rpointMethod f = f :& rpointMethod @c f
  {-# INLINE rpointMethod #-}

-- | Apply a typeclass method to each field of a 'Rec'.
class RecMapMethod c (f :: u -> *) (ts :: [u]) where
  rmapMethod :: (forall a. c (PayloadType f a) => f a -> g a)
             -> Rec f ts -> Rec g ts

instance RecMapMethod c f '[] where
  rmapMethod _ RNil = RNil
  {-# INLINE rmapMethod #-}

instance (c (PayloadType f t), RecMapMethod c f ts)
  => RecMapMethod c f (t ': ts) where
  rmapMethod f (x :& xs) = f x :& rmapMethod @c f xs
  {-# INLINE rmapMethod #-}

-- | Apply a typeclass method to each field of a @Rec f ts@ using the
-- 'Functor' instance for @f@ to lift the function into the
-- functor. This is a commonly-used specialization of 'rmapMethod'
-- composed with 'fmap'.
rmapMethodF :: forall c f ts. (Functor f, FieldPayload f ~ 'FieldId, RecMapMethod c f ts)
            => (forall a. c a => a -> a) -> Rec f ts -> Rec f ts
rmapMethodF f = rmapMethod @c (fmap f)
{-# INLINE rmapMethodF #-}

-- | Apply a typeclass method to each field of a 'FieldRec'. This is a
-- specialization of 'rmapMethod'.
mapFields :: forall c ts. RecMapMethod c ElField ts
           => (forall a. c a => a -> a) -> FieldRec ts -> FieldRec ts
mapFields f = rmapMethod @c g
  where g :: c (PayloadType ElField t) => ElField t -> ElField t
        g (Field x) = Field (f x)
{-# INLINE mapFields #-}

{- $example
    This module provides variants of typeclass methods that have
    a 'RecAll' constraint instead of the normal typeclass
    constraint. For example, a type-specialized 'compare' would
    look like this:

> compare :: Ord (Rec f rs) => Rec f rs -> Rec f rs -> Ordering

    The 'recCompare' function looks like this:

> recCompare :: RecAll f rs Ord => Rec f rs -> Rec f rs -> Ordering

    The only difference is the constraint. Let's look at a potential
    use case for these functions.

    Let's write a function that projects out a subrecord from two records and
    then compares those for equality. We can write this with
    the '<:' operator from @Data.Vinyl.Lens@ and the normal 'compare'
    function. We don't need 'recCompare':

> -- This needs ScopedTypeVariables
> projectAndCompare :: forall super sub f. (super <: sub, Ord (Rec f sub))
>                   => Proxy sub -> Rec f super -> Rec f super -> Ordering
> projectAndCompare _ a b = compare (rcast a :: Rec f sub) (rcast b :: Rec f sub)

    That works fine for the majority of use cases, and it is probably how you should
    write the function if it does everything you need. However, let's consider
    a somewhat more complicated case.

    What if the exact subrecord we were projecting couldn't be
    known at compile time? Assume that the end user was allowd to
    choose the fields on which he or she wanted to compare records.
    The @projectAndCompare@ function cannot handle this because of the
    @Ord (Rec f sub)@ constraint. Even if we amend the constraint to
    read @Ord (Rec f super)@ instead, we cannot use this information
    to recover the @Ord (Rec f sub)@ constraint that we need. Let's
    try another approach.

    We can use the following GADT to prove subsethood:

> data Sublist (super :: [k]) (sub :: [k]) where
>   SublistNil   :: Sublist '[]
>   SublistSuper :: Proxy r -> Sublist super sub -> Sublist (r ': super) sub
>   SublistBoth  :: Proxy r -> Sublist super sub -> Sublist (r ': super) (r ': sub)
>
> projectRec :: Sublist super sub -> Rec f super -> Rec f sub
> projectRec s r = case s of
>   SublistNil -> RNil
>   SublistBoth n snext -> case r of
>     rhead :& rtail -> rhead :& projectRec snext rtail
>   SublistSuper n snext -> case r of
>     rhead :& rtail -> projectRec snext rtail

    It is also possible to write a typeclass to generate @Sublist@s
    implicitly, but that is beyond the scope of this example. Let's
    now write a function to use @Sublist@ to weaken a 'RecAll'
    constraint:

> import Data.Vinyl.Core hiding (Dict)
> import Data.Constraint
>
> weakenRecAll :: Proxy f -> Proxy c -> Sublist super sub -> RecAll f super c :- RecAll f sub c
> weakenRecAll f c s = case s of
>   SublistNil -> Sub Dict
>   SublistSuper _ snext -> Sub $ case weakenRecAll f c snext of
>     Sub Dict -> Dict
>   SublistBoth _ snext -> Sub $ case weakenRecAll f c snext of
>     Sub Dict -> Dict

    Now we can write a different version of our original function:

> -- This needs ScopedTypeVariables
> projectAndCompare2 :: forall super sub f. (RecAll f super Ord)
>                    => Sublist super sub -> Rec f super -> Rec f super -> Ordering
> projectAndCompare2 s a b = case weakenRecAll (Proxy :: Proxy f) (Proxy :: Proxy Ord) s of
>   Sub Dict -> recCompare (projectRec s a) (projectRec s b)

    Notice that in this case, the 'Ord' constraint applies to the full set of fields
    and is then weakened to target a subset of them instead.
-}
