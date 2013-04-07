{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Validation where

import           Control.Applicative
import           Control.Monad.Identity
import           Data.Monoid
import           Data.Vinyl.Classes

-- | A type which is similar to 'Either', except that it has a
-- slightly different Applicative instance.
data Result e a
  = Failure e
  | Success a
  deriving (Show, Eq)

-- | Validators transform identities into results.
type Validator e = Identity ~> Result e

instance Functor (Result e) where
  fmap f (Success x) = Success $ f x
  fmap f (Failure e) = Failure e

-- | The 'Applicative' instance to 'Result' relies on its error type
-- being a 'Monoid'. That way, it can accumulate errors.
instance Monoid e => Applicative (Result e) where
  pure = Success
  (Success f) <*> (Success x)  = Success $ f x
  (Failure e) <*> (Success x)  = Failure e
  (Success f) <*> (Failure e)  = Failure e
  (Failure e) <*> (Failure e') = Failure $ e <> e'
