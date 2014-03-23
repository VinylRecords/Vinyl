{-# LANGUAGE DeriveFunctor #-}

module Data.Vinyl.Idiom.Identity where

import Control.Applicative

newtype Identity a
  = Identity
  { runIdentity :: a
  } deriving Functor

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x

instance Show a => Show (Identity a) where
  show (Identity x) = show x
