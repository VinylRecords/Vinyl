{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Safe                  #-}
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

-- | If a record is homogenous, you can fold over it.
class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b

-- | '(~>)' is a morphism between functors.
newtype (f ~> g) x = NT { runNT :: f x -> g x }

