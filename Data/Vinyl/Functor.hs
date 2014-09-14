{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Vinyl.Functor where

import Control.Applicative
import Data.Foldable
import Data.Traversable

newtype Identity a
  = Identity
  { getIdentity :: a
  } deriving (Functor, Foldable, Traversable)

data Thunk a
  = Thunk
  { getThunk :: a
  } deriving (Functor, Foldable, Traversable)

newtype Lift (op :: l -> l' -> *) (f :: k -> l) (g :: k -> l') (x :: k)
  = Lift
  { getLift :: op (f x) (g x)
  }

newtype Compose f g x
  = Compose
  { getCompose :: f (g x)
  }
type f :. g = Compose f g

newtype Const a b
  = Const
  { getConst :: a
  } deriving (Functor, Foldable, Traversable)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

instance Show a => Show (Identity a) where
  show (Identity x) = show x

instance Applicative Thunk where
  pure = Thunk
  (Thunk f) <*> (Thunk x) = Thunk (f x)

instance Monad Thunk where
  return = Thunk
  (Thunk x) >>= f = f x

instance Show a => Show (Thunk a) where
  show (Thunk x) = show x

instance (Functor f, Functor g) => Functor (Lift (,) f g) where
  fmap f (Lift (x, y)) = Lift (fmap f x, fmap f y)

instance (Functor f, Functor g) => Functor (Lift Either f g) where
  fmap f (Lift (Left x)) = Lift . Left . fmap f $ x
  fmap f (Lift (Right x)) = Lift . Right . fmap f $ x

instance (Applicative f, Applicative g) => Applicative (Lift (,) f g) where
  pure x = Lift (pure x, pure x)
  Lift (f, g) <*> Lift (x, y) = Lift (f <*> x, g <*> y)

