{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Classes where

import           Control.Applicative
import           Control.Monad.Identity

class Apply (arr :: k -> k -> k) (f :: k -> *) where
  (<<*>>) :: f (arr a b) -> f a -> f b

class Run t where
  run :: Applicative f => t f -> f (t Identity)

newtype (f ~> g) x = NT { runNT :: f x -> g x }

