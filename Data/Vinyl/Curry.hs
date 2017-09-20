{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

{-|

Provides combinators for currying and uncurrying functions over arbitrary vinyl
records.

-}
module Data.Vinyl.Curry where

import           Data.Vinyl
import           Data.Vinyl.Functor

-- * Currying

class RecordCurry ts where

  -- | For the type-level list @ts@, @'CurriedF' f ts a@ is a curried function
  -- from arguments of type @f t@ for @t@ in @ts@, to a result of type @a@. E.g.
  --
  -- @
  -- CurriedF 'Maybe' '['Int', 'Bool', 'String'] 'Int' ~ ('Maybe' 'Int' -> 'Maybe' 'Bool' -> 'Maybe' 'String' -> 'Int')
  -- @
  type CurriedF (f :: * -> *) ts a

  -- | For the list of types @ts@, @'Curried' ts a@ is a curried function from
  -- arguments of types in @ts@ to a result of type @a@. E.g.
  --
  -- @
  -- Curried '['Int', 'Bool', 'String'] 'Int' ~ '('Int -> 'Bool' -> 'String' -> 'Int')
  -- @
  type Curried ts a

  -- | N-ary version of 'curry' over functorial records.
  rcurry :: (Rec f ts -> a) -> CurriedF f ts a

  -- | N-ary version of 'curry' over pure records.
  rcurry' :: (Rec Identity ts -> a) -> Curried ts a


instance RecordCurry '[] where
  type CurriedF f '[] a = a
  type Curried '[] a = a

  rcurry f = f RNil
  rcurry' f = f RNil

instance RecordCurry ts => RecordCurry (t ': ts) where
  type CurriedF f (t ': ts) a = f t -> CurriedF f ts a
  type Curried (t ': ts) a = t -> Curried ts a

  rcurry f x = rcurry (\xs -> f (x :& xs))
  rcurry' f x = rcurry' (\xs -> f (Identity x :& xs))

-- * Uncurrying

-- | N-ary version of 'uncurry' over functorial records.
runcurry :: CurriedF f ts a -> Rec f ts -> a
runcurry x RNil = x
runcurry f (x :& xs) = runcurry (f x) xs


{-|
N-ary version of 'uncurry' over pure records.

@
f :: 'Rec' 'Identity' '['Bool', 'Int', 'Double'] -> 'Either' 'Int' 'Double'
f = runcurry' $ \b x y -> if b then 'Left' x else 'Right' y
@
-}
runcurry' :: Curried ts a -> Rec Identity ts -> a
runcurry' x RNil = x
runcurry' f (Identity x :& xs) = runcurry' (f x) xs

-- * Applicative Combinators

-- | N-ary version of 'Control.Applicative.liftA2' over records where the
-- functor is 'Applicative'.
rliftA :: (Applicative f) => (Rec Identity ts -> a) -> Rec f ts -> f a
rliftA = go . pure
  where
    go :: (Applicative f) => f (Rec Identity ts -> a) -> Rec f ts -> f a
    go f RNil = f <*> pure RNil
    go f (x :& xs) = go ((\f' x' xs' -> f' (Identity x' :& xs')) <$> f <*> x) xs


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
