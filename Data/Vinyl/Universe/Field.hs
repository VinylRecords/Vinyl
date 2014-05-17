{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Universe.Field where

import Data.Vinyl.TyFun
import GHC.TypeLits

data (sy :: k) ::: (t :: *)

data SField :: * -> * where
  SField :: KnownSymbol sy => SField (sy ::: t)

data ElField :: (TyFun * *) -> * where
  ElField :: ElField el
type instance App ElField (sy ::: t) = t
