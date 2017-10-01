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
  {-|
  N-ary version of 'curry' over functorial records.

  Example specialized signatures:

  @
  rcurry :: (Rec Maybe '[Int, Double] -> Bool) -> Maybe Int -> Maybe Double -> Bool
  rcurry :: (Rec (Either Int) '[Double, String, ()] -> Int) -> Either Int Double -> Either Int String -> Either Int () -> Int
  rcurry :: (Rec f '[] -> Bool) -> Bool
  @

  -}
  rcurry :: (Rec f ts -> a) -> CurriedF f ts a

  {-|
  N-ary version of 'curry' over pure records.

  Example specialized signatures:

  @
  rcurry' :: (Rec Identity '[Int, Double] -> Bool) -> Int -> Double -> Bool
  rcurry' :: (Rec Identity '[Double, String, ()] -> Int) -> Double -> String -> () -> Int
  rcurry' :: (Rec Identity '[] -> Bool) -> Bool
  @

  -}
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

{-|
N-ary version of 'uncurry' over functorial records.

Example specialized signatures:

@
runcurry :: (Maybe Int -> Maybe Double -> String) -> Rec Maybe '[Int, Double] -> String
runcurry :: (IO FilePath -> String) -> Rec IO '[FilePath] -> String
runcurry :: Int -> Rec f '[] -> Int
@
-}
runcurry :: CurriedF f ts a -> Rec f ts -> a
runcurry x RNil      = x
runcurry f (x :& xs) = runcurry (f x) xs
{-# INLINABLE runcurry #-}


{-|
N-ary version of 'uncurry' over pure records.

Example specialized signatures:

@
runcurry' :: (Int -> Double -> String) -> Rec Identity '[Int, Double] -> String
runcurry' :: Int -> Rec Identity '[] -> Int
@

Example usage:

@
f :: Rec Identity '[Bool, Int, Double] -> Either Int Double
f = runcurry' $ \b x y -> if b then Left x else Right y
@
-}
runcurry' :: Curried ts a -> Rec Identity ts -> a
runcurry' x RNil               = x
runcurry' f (Identity x :& xs) = runcurry' (f x) xs
{-# INLINABLE runcurry' #-}

-- * Applicative Combinators

{-|
Lift an N-ary function to work over a record of 'Applicative' computations.

>>> runcurryA' (+) (Just 2 :& Just 3 :& RNil)
Just 5

>>> runcurryA' (+) (Nothing :& Just 3 :& RNil)
Nothing
-}
runcurryA' :: (Applicative f) => Curried ts a -> Rec f ts -> f a
runcurryA' f = fmap (runcurry' f) . rtraverse (fmap Identity)
{-# INLINE runcurryA' #-}

{-|
Lift an N-ary function over types in @g@ to work over a record of 'Compose'd
'Applicative' computations. A more general version of 'runcurryA''.

Example specialized signatures:

@
runcurryA :: (g x -> g y -> a) -> Rec (Compose Maybe g) '[x, y] -> Maybe a
@
-}
runcurryA :: (Applicative f) => CurriedF g ts a -> Rec (Compose f g) ts -> f a
runcurryA f = fmap (runcurry f) . rtraverse getCompose
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
