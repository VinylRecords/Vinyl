{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Vinyl.Derived where

import Data.Proxy
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Lens
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.OverloadedLabels
import GHC.TypeLits

-- | Alias for Field spec
type a ::: b = '(a, b)

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

infix 3 =:
(=:) :: KnownSymbol l => Label (l :: Symbol) -> (v :: *) -> ElField (l ::: v)
_ =: v = Field v

rgetf
  :: forall l f v i us. HasValue l us v i
  => Label l -> Rec f us -> f (l ::: v)
rgetf _ = rget (Proxy :: Proxy (l ::: v))

rvalf
  :: HasValue l us v i
  => Label l -> Rec ElField us -> v
rvalf x = getField . rgetf x

-- | Shorthand for a 'FieldRec' with a single field.
(=:=) :: KnownSymbol s => proxy '(s,a) -> a -> FieldRec '[ '(s,a) ]
(=:=) _ x = Field x :& RNil

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

type family FindValue l fs where
  FindValue l '[] = TypeError ('Text "Cannot find label "
                               ':<>: 'ShowType l
                               ':<>: 'Text " in fields")
  FindValue l ((l ::: v) ': fs) = v
  FindValue l ((l' ::: v') ': fs) = FindValue l fs
type HasValue l fs v i = (RElem (l ::: v) fs i, FindValue l fs ~ v)

-- proxy for label type
data Label (a :: Symbol) = Label
  deriving (Eq, Show)

instance s ~ s' => IsLabel s (Label s') where
  fromLabel _ = Label

-- rlabels :: Rec (Const String) us
class GetLabels fs where
  rlabels :: Rec (Const String) fs

instance GetLabels '[] where
  rlabels = RNil

instance (KnownSymbol l, GetLabels fs) => GetLabels ((l ::: v) ': fs) where
  rlabels = Const (symbolVal (Proxy :: Proxy l)) :& rlabels
