{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Functor where

import Control.Applicative
import Data.Vinyl.TyFun

class Presheaf f where
  contramap :: (a -> b) -> (f b -> f a)

newtype Lift op f g x = Lift { runLift :: op (f $ x) (g $ x) }

instance (Functor f, Functor g) => Functor (Lift (,) (TC f) (TC g)) where
  fmap f (Lift (x, y)) = Lift (fmap f x, fmap f y)

instance (Functor f, Functor g) => Functor (Lift Either (TC f) (TC g)) where
  fmap f (Lift (Left x)) = Lift . Left . fmap f $ x
  fmap f (Lift (Right x)) = Lift . Right . fmap f $ x

instance (Presheaf f, Presheaf g) => Presheaf (Lift (,) (TC f) (TC g)) where
  contramap f (Lift (x, y)) = Lift (contramap f x, contramap f y)

instance (Presheaf f, Presheaf g) => Presheaf (Lift Either (TC f) (TC g)) where
  contramap f (Lift (Left x)) = Lift . Left . contramap f $ x
  contramap f (Lift (Right x)) = Lift . Right . contramap f $ x

instance (Applicative f, Applicative g) => Applicative (Lift (,) (TC f) (TC g)) where
  pure x = Lift (pure x, pure x)
  Lift (f, g) <*> Lift (x, y) = Lift (f <*> x, g <*> y)

instance (Presheaf f, Functor g) => Functor (Lift (->) (TC f) (TC g)) where
  fmap f (Lift ηx) = Lift $ fmap f . ηx . contramap f

instance (Functor f, Presheaf g) => Presheaf (Lift (->) (TC f) (TC g)) where
  contramap f (Lift ηx) = Lift $ contramap f . ηx . fmap f

