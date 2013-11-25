{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Vinyl.Classes where

import           Control.Applicative
-- import           Control.Functor.Apply
import           Data.Functor.Identity

class ApFunctor (f :: (k -> *) -> *) where
    (<<$>>) :: forall (g :: k -> *) (h :: k -> *). 
              (forall (x :: k). g x -> h x) -> f g -> f h

class (ApFunctor f) => ApTraversable (f :: (k -> *) -> *) where
    apTraverse :: forall (g :: k -> *) (h :: k -> *) (e :: * -> *).
                  (Applicative e) =>
                  (forall (x :: k). g x -> e (h x)) -> f g -> e (f h)

-- class (ApTraversable f) => ApTraversable1 (f :: (k -> *) -> *) where
    -- apTraverse1 :: forall (g :: k -> *) (h :: k -> *) (e :: * -> *). 
                   -- (Apply e) => 
                   -- (forall (x :: k). g x -> e (h x)) -> f g -> e (f h)

class (ApFunctor f) => ApAlt (f :: (k -> *) -> *) where
    (<<|>>) :: forall (g :: k -> *). f g -> f g -> f g

class (ApFunctor f) => ApEmpty (f :: (k -> *) -> *) where
    apEmpty :: forall (g :: k -> *). f g

-- class (ApAlt f, ApEmpty f) => ApAlternative (f :: (k -> *) -> *)
-- instance (ApAlt f, ApEmpty f) => ApAlternative (f :: (k -> *) -> *)
type ApAlternative f = (ApAlt f, ApEmpty f)

class (ApFunctor f) => ApApply (arr :: ((k -> *) -> (k -> *) -> (k -> *)))
                               (f :: (k -> *) -> *) where
    (<<*>>) :: forall (g :: k -> *) (h :: k -> *). f (arr g h) -> f g -> f h
-- | This class is a generalized, but non-pointed version of 'Applicative'. This
-- is useful for types which range over functors rather than sets.
-- class Apply (arr :: k -> k -> k) (f :: k -> *) where
  -- (<<*>>) :: f (arr a b) -> f a -> f b

-- -- | To accumulate effects distributed over a data type, you 'dist' it.
-- -- This class is a generalized version of 'Traversable'.
-- class Dist t where
  -- dist :: Applicative f => (forall x. a x -> f (b x)) -> t a -> f (t b)

-- run :: (Applicative f, Dist t) => t f -> f (t Identity)
-- run = dist (Identity <$>)

-- | If a record is homogenous, you can fold over it.
class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b

-- | '(~>)' is a morphism between functors.
newtype (f ~> g) x = NT { runNT :: f x -> g x }

-- -- | This class is a generalized version of 'Alternative', analogous to 'Apply'.
-- class Alternate (f :: (* -> *) -> *) where
  -- eemptyy :: Alternative g => f g
  -- (<<|>>) :: Alternative g => f g -> f g -> f g

-- -- | This class is a generalized version of 'Functor'. This is useful for types
-- -- which range over functors rather than sets.
-- class Funct f where
  -- (<<$>>) :: (forall x. g x -> h x) -> f g -> f h
