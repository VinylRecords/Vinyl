{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Records.Rec where

import Data.Records.Field
import GHC.TypeLits

-- A record is parameterized by a list of fields.
data Rec :: [*] -> * where
  RNil :: Rec '[]
  (:&) :: (f ~ (sy ::: t)) => (f, t) -> Rec fs -> Rec (f ': fs)
infixr :&

instance Show (Rec '[]) where
  show RNil = "{}"
instance (SingI sy, Show t, Show (Rec fs)) => Show (Rec ((sy ::: t) ': fs)) where
  show ((k,x) :& xs) = show k ++ " :=: " ++ show x ++ ", " ++ show xs

