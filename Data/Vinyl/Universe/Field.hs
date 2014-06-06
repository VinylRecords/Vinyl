{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Universe.Field where

import Data.Vinyl.TyFun
import GHC.TypeLits

data (sy :: k) ::: (t :: *)

#if __GLASGOW_HASKELL__ > 707
data SField :: * -> * where
  SField :: KnownSymbol sy => SField (sy ::: t)
#else
data SField :: * -> * where
  SField :: SingE (sy :: Symbol) str => SField (sy ::: t)
#endif

data ElField :: (TyFun * *) -> * where
  ElField :: ElField el
type instance App ElField (sy ::: t) = t
