{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Validation where

import Data.Vinyl.Classes
import Control.Applicative
import Control.Monad.Identity
import Data.Monoid

data Result e a
  = Failure e
  | Success a
  deriving (Show, Eq)

type Validator e = Identity ~> Result e

instance Functor (Result e) where
  fmap f (Success x) = Success (f x)
  fmap f (Failure e) = Failure e

instance Monoid e => Applicative (Result e) where
  pure = Success
  (Success f) <*> (Success x)  = Success (f x)
  (Failure e) <*> (Success x)  = Failure e
  (Success f) <*> (Failure e)  = Failure e
  (Failure e) <*> (Failure e') = Failure (e <> e')
