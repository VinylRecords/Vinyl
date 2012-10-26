{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.Records.Field where

import GHC.TypeLits

-- A field is a symbol key and a type for its value.
data (:::) :: Symbol -> * -> * where
  Field :: sy ::: t

instance (SingI sy, Show t) => Show (sy ::: t) where
  show Field = fromSing (sing :: Sing sy)
