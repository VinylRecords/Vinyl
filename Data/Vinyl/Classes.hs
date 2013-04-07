{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Classes where

import           Control.Applicative
import           Control.Monad.Identity

-- | This class is a generalized, but non-pointed version of 'Applicative'. This
-- is useful for types which range over functors rather than sets.
class Apply (arr :: k -> k -> k) (f :: k -> *) where
  (<<*>>) :: f (arr a b) -> f a -> f b

-- | To accumulate effects distributed over a data type, you 'run' it.
class Run t where
  run :: Applicative f => t f -> f (t Identity)

-- | '(~>)' is a morphism between functors.
newtype (f ~> g) x = NT { runNT :: f x -> g x }

