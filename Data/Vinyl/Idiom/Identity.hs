{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Vinyl.Idiom.Identity where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Typeable

newtype Identity a
  = Identity
  { runIdentity :: a
  } deriving (Functor, Foldable, Traversable, Typeable)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x

instance Show a => Show (Identity a) where
  show (Identity x) = show x
