{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Vinyl.Field where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Data.Proxy
#endif
import           GHC.TypeLits

-- | A field contains a key and a type.
data (:::) :: Symbol -> * -> * where
  Field :: sy ::: t

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
instance KnownSymbol sy => Show (sy ::: t) where
  show Field = symbolVal (Proxy :: Proxy sy)
#else
instance SingI sy => Show (sy ::: t) where
  show Field = fromSing (sing :: Sing sy)
#endif
