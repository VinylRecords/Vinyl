{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Vinyl.Derived where

import Data.Proxy
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.TypeLits

data ElField (field :: (Symbol, *)) where
  Field :: KnownSymbol s => !t -> ElField '(s,t)

type FieldRec = Rec ElField
type HList = Rec Identity
type LazyHList = Rec Thunk

deriving instance Eq t => Eq (ElField '(s,t))
deriving instance Ord t => Ord (ElField '(s,t))

instance Show t => Show (ElField '(s,t)) where
  show (Field x) = (symbolVal (Proxy::Proxy s))++" :-> "++show x

-- | Get the data payload of an 'ElField'.
getField :: ElField '(s,t) -> t
getField (Field x) = x

-- | 'ElField' is isomorphic to a functor something like @Compose
-- ElField ('(,) s)@.
fieldMap :: (a -> b) -> ElField '(s,a) -> ElField '(s,b)
fieldMap f (Field x) = Field (f x)
{-# INLINE fieldMap #-}

-- | Lens for an 'ElField''s data payload.
rfield :: Functor f => (a -> f b) -> ElField '(s,a) -> f (ElField '(s,b))
rfield f (Field x) = fmap Field (f x)
{-# INLINE rfield #-}

-- | Shorthand for a 'FieldRec' with a single field.
(=:) :: KnownSymbol s => proxy '(s,a) -> a -> FieldRec '[ '(s,a) ]
(=:) _ x = Field x :& RNil

-- | A proxy for field types.
data SField (field :: k) = SField

instance Eq (SField a) where _ == _ = True
instance Ord (SField a) where compare _ _ = EQ
instance KnownSymbol s => Show (SField '(s,t)) where
  show _ = "SField "++symbolVal (Proxy::Proxy s)

instance forall s t. (KnownSymbol s, Storable t)
    => Storable (ElField '(s,t)) where
  sizeOf _ = sizeOf (undefined::t)
  alignment _ = alignment (undefined::t)
  peek ptr = Field `fmap` peek (castPtr ptr)
  poke ptr (Field x) = poke (castPtr ptr) x
