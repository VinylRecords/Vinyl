{-# LANGUAGE DeriveFunctor #-}

module Data.Vinyl.Idiom.LazyIdentity where

import Control.Applicative

data LazyIdentity a
  = LazyIdentity
  { runLazyIdentity :: a
  } deriving Functor

instance Applicative LazyIdentity where
  pure = LazyIdentity
  (LazyIdentity f) <*> (LazyIdentity x) = LazyIdentity (f x)

instance Monad LazyIdentity where
  return = LazyIdentity
  (LazyIdentity x) >>= f = f x

instance Show a => Show (LazyIdentity a) where
  show (LazyIdentity x) = show x
