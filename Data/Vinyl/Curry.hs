{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|

Provides combinators for currying and uncurrying functions over arbitrary vinyl
records.

-}
module Data.Vinyl.Curry where

import           Data.Vinyl
import           Data.Vinyl.Functor

-- * Currying

class RecordCurry ts where
  -- | N-ary version of 'curry' over functorial records.
  rcurry :: (Rec f ts -> a) -> CurriedF f ts a

  -- | N-ary version of 'curry' over pure records.
  rcurry' :: (Rec Identity ts -> a) -> Curried ts a


instance RecordCurry '[] where
  rcurry f = f RNil
  {-# INLINABLE rcurry #-}
  rcurry' f = f RNil
  {-# INLINABLE rcurry' #-}

instance RecordCurry ts => RecordCurry (t ': ts) where
  rcurry f x = rcurry (\xs -> f (x :& xs))
  {-# INLINABLE rcurry #-}
  rcurry' f x = rcurry' (\xs -> f (Identity x :& xs))
  {-# INLINABLE rcurry' #-}

-- * Uncurrying

-- | N-ary version of 'uncurry' over functorial records.
runcurry :: CurriedF f ts a -> Rec f ts -> a
runcurry x RNil      = x
runcurry f (x :& xs) = runcurry (f x) xs
{-# INLINABLE runcurry #-}


{-|
N-ary version of 'uncurry' over pure records.

@
f :: 'Rec' 'Identity' '['Bool', 'Int', 'Double'] -> 'Either' 'Int' 'Double'
f = runcurry' $ \b x y -> if b then 'Left' x else 'Right' y
@
-}
runcurry' :: Curried ts a -> Rec Identity ts -> a
runcurry' x RNil               = x
runcurry' f (Identity x :& xs) = runcurry' (f x) xs
{-# INLINABLE runcurry' #-}

-- * Applicative Combinators

-- | N-ary version of 'Control.Applicative.liftA2' over records where the
-- functor is 'Applicative'.
rliftA :: (Applicative f) => (Rec Identity ts -> a) -> Rec f ts -> f a
rliftA f = rliftComposeA f . rmap (Compose . fmap Identity)
{-# INLINE rliftA #-}


-- | Generalized version of 'rliftA' where the input function can accept a
-- record over an arbitrary functor.
rliftComposeA :: (Applicative f) => (Rec g ts -> a) -> Rec (Compose f g) ts -> f a
rliftComposeA = go . pure
  where
    go :: (Applicative f) => f (Rec g ts -> a) -> Rec (Compose f g) ts -> f a
    go f RNil = f <*> pure RNil
    go f (Compose x :& xs) = go ((\f' x' xs' -> f' (x' :& xs')) <$> f <*> x) xs
    {-# INLINABLE go #-}
{-# INLINE rliftComposeA #-}


{-|
Lift an N-ary function to work over a record of 'Applicative' computations.

@
runcurryA = 'rliftA' '.' 'runcurry''
@

>>> runcurryA (+) (Just 2 :& Just 3 :& RNil)
Just 5

>>> runcurryA (+) (Nothing :& Just 3 :& RNil)
Nothing
-}
runcurryA :: (Applicative f, RecordCurry ts) => Curried ts a -> Rec f ts -> f a
runcurryA = rliftA . runcurry'
{-# INLINE runcurryA #-}

-- * Curried Function Types

{-|
For the list of types @ts@, @'Curried' ts a@ is a curried function type from
arguments of types in @ts@ to a result of type @a@.

>>> :kind! Curried '[Int, Bool, String] Int
Curried '[Int, Bool, String] Int :: *
= Int -> Bool -> [Char] -> Int
-}
type family Curried ts a where
  Curried '[] a = a
  Curried (t ': ts) a = t -> Curried ts a


{-|
For the type-level list @ts@, @'CurriedF' f ts a@ is a curried function type
from arguments of type @f t@ for @t@ in @ts@, to a result of type @a@.

>>> :kind! CurriedF Maybe '[Int, Bool, String] Int
CurriedF Maybe '[Int, Bool, String] Int :: *
= Maybe Int -> Maybe Bool -> Maybe [Char] -> Int
-}
type family CurriedF (f :: u -> *) (ts :: [u]) a where
  CurriedF f '[] a = a
  CurriedF f (t ': ts) a = f t -> CurriedF f ts a
