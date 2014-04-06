{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Universe.Field where

import Data.Vinyl.TyFun
import GHC.TypeLits

data (sy :: k) ::: (t :: *)
data instance Sing (sy ::: t) = Field (Sing sy)

type family ElField_ (r :: *) :: *
type instance ElField_ (sy ::: t) = t
data ElField :: (TyFun * *) -> *
type instance ElField $ r = ElField_ r
