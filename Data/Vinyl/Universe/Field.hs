{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Universe.Field where

import Data.Vinyl.TyFun
import GHC.TypeLits

data (sy :: k) ::: (t :: *)
data instance Sing (sy ::: t) = Field (Sing sy)

data ElField :: (TyFun * *) -> *
type instance ElField $ (sy ::: t) = t
