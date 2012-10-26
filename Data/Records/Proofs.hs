{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Records.Proofs where

class Implicit p where
  implicitly :: p

data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There implicitly

type IElem x xs = Implicit (Elem x xs)
