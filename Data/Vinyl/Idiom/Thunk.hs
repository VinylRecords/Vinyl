{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Vinyl.Idiom.Thunk where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Typeable

data Thunk a
  = Thunk
  { runThunk :: a
  } deriving (Functor, Foldable, Traversable, Typeable)

instance Applicative Thunk where
  pure = Thunk
  (Thunk f) <*> (Thunk x) = Thunk (f x)

instance Monad Thunk where
  return = Thunk
  (Thunk x) >>= f = f x

instance Show a => Show (Thunk a) where
  show (Thunk x) = show x
