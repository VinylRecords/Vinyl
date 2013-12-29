{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SimpleLens (simpleLens) where

import Data.Vinyl.Field
import Data.Vinyl.Rec

simpleLens :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
simpleLens _ f = go implicitly
  where go :: Elem r rr -> Rec rr f -> g (Rec rr f)
        go Here (x :& xs) = fmap (:& xs) (f x)
        go (There p) (x :& xs) = fmap (x :&) (go p xs)
        {-# INLINABLE go #-}
{-# INLINE simpleLens #-}

class Implicit p where
  implicitly :: p

data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

-- | A constraint for implicit resolution of list membership proofs.
type IElem x xs = Implicit (Elem x xs)

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There implicitly
