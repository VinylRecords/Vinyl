{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Vinyl.Field where

import           GHC.TypeLits

-- | A field contains a key and a type.
data (:::) :: Symbol -> * -> * where
  Field :: sy ::: t

instance SingI sy => Show (sy ::: t) where
  show Field = fromSing (sing :: Sing sy)
