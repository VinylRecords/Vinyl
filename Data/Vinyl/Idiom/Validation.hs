{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Idiom.Validation where

import Data.Typeable
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Functor

import Control.Applicative
import Data.Monoid

-- | A type which is similar to 'Either', except that it has a
-- slightly different Applicative instance.
data Result e a
  = Failure e
  | Success a
  deriving (Show, Eq, Typeable)

-- | Validators transform identities into results.
type Validator e = Lift (->) Identity (Result e)

instance Functor (Result e) where
  fmap f (Success x) = Success $ f x
  fmap _ (Failure e) = Failure e

-- | The 'Applicative' instance to 'Result' relies on its error type
-- being a 'Monoid'. That way, it can accumulate errors.
instance Monoid e => Applicative (Result e) where
  pure = Success
  (Success f) <*> (Success x)  = Success $ f x
  (Failure e) <*> (Success _)  = Failure e
  (Success _) <*> (Failure e)  = Failure e
  (Failure e) <*> (Failure e') = Failure $ e <> e'
