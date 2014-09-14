{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Witnesses where

class Implicit p where
  implicitly :: p

-- | An inductive list membership proposition.
data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

-- | A constraint for implicit resolution of list membership proofs.
type IElem x xs = Implicit (Elem x xs)
type x ∈ xs = IElem x xs

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There implicitly
