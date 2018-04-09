{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeApplications #-}
-- | Commonly used 'Rec' instantiations.
module Data.Vinyl.Derived where

import Data.Proxy
import Data.Vinyl.ARec
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Fst, Snd, AllConstrained, RIndex)
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.OverloadedLabels
import GHC.TypeLits

-- | Alias for Field spec
type a ::: b = '(a, b)

data ElField (field :: (Symbol, *)) where
  Field :: KnownSymbol s => !t -> ElField '(s,t)

-- | A record of named fields.
type FieldRec = Rec ElField

-- | An 'ARec' of named fields to provide constant-time field access.
type AFieldRec ts = ARec ElField ts

-- | Heterogeneous list whose elements are evaluated during list
-- construction.
type HList = Rec Identity

-- | Heterogeneous list whose elements are left as-is during list
-- construction (cf. 'HList').
type LazyHList = Rec Thunk

deriving instance Eq t => Eq (ElField '(s,t))
deriving instance Ord t => Ord (ElField '(s,t))

instance Show t => Show (ElField '(s,t)) where
  show (Field x) = symbolVal (Proxy::Proxy s) ++" :-> "++show x

-- | Get the data payload of an 'ElField'.
getField :: ElField '(s,t) -> t
getField (Field x) = x

-- | Get the label name of an 'ElField'.
getLabel :: forall s t. ElField '(s,t) -> String
getLabel (Field _) = symbolVal (Proxy::Proxy s)

-- | 'ElField' is isomorphic to a functor something like @Compose
-- ElField ('(,) s)@.
fieldMap :: (a -> b) -> ElField '(s,a) -> ElField '(s,b)
fieldMap f (Field x) = Field (f x)
{-# INLINE fieldMap #-}

-- | Lens for an 'ElField''s data payload.
rfield :: Functor f => (a -> f b) -> ElField '(s,a) -> f (ElField '(s,b))
rfield f (Field x) = fmap Field (f x)
{-# INLINE rfield #-}

infix 8 =:

-- | Operator for creating an 'ElField'. With the @-XOverloadedLabels@
-- extension, this permits usage such as, @#foo =: 23@ to produce a
-- value of type @ElField ("foo" ::: Int)@.
(=:) :: KnownSymbol l => Label (l :: Symbol) -> (v :: *) -> ElField (l ::: v)
_ =: v = Field v

-- | Get a named field from a record.
rgetf
  :: forall l f v record us. HasField record l us v
  => Label l -> record f us -> f (l ::: v)
rgetf _ = rget (Proxy :: Proxy (l ::: v))

-- | Get the value associated with a named field from a record.
rvalf
  :: HasField record l us v => Label l -> record ElField us -> v
rvalf x = getField . rgetf x

-- | Set a named field. @rputf #foo 23@ sets the field named @#foo@ to
-- @23@.
rputf :: forall l v record us. (HasField record l us v, KnownSymbol l)
      => Label l -> v -> record ElField us -> record ElField us
rputf _ = rput . (Field :: v -> ElField '(l,v))

-- | A lens into a 'Rec' identified by a 'Label'.
rlensf' :: forall l v record g f us. (Functor g, HasField record l us v)
        => Label l
        -> (f (l ::: v) -> g (f (l ::: v)))
        -> record f us
        -> g (record f us)
rlensf' _ f = rlens (Proxy :: Proxy (l ::: v)) f

-- | A lens into the payload value of a 'Rec' field identified by a
-- 'Label'.
rlensf :: forall l v record g us.
          (Functor g, HasField record l us v, RecElemFCtx record ElField)
       => Label l -> (v -> g v) -> record ElField us -> g (record ElField us)
rlensf _ f = rlens (Proxy :: Proxy (l ::: v)) (rfield f)

-- | Shorthand for a 'FieldRec' with a single field.
(=:=) :: KnownSymbol s => Label (s :: Symbol) -> a -> FieldRec '[ '(s,a) ]
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

type family FieldType l fs where
  FieldType l '[] = TypeError ('Text "Cannot find label "
                               ':<>: 'ShowType l
                               ':<>: 'Text " in fields")
  FieldType l ((l ::: v) ': fs) = v
  FieldType l ((l' ::: v') ': fs) = FieldType l fs

-- | Constraint that a label is associated with a particular type in a
-- record.
type HasField record l fs v =
  (RecElem record (l ::: v) fs (RIndex (l ::: v) fs), FieldType l fs ~ v)

-- | Proxy for label type
data Label (a :: Symbol) = Label
  deriving (Eq, Show)

instance s ~ s' => IsLabel s (Label s') where
#if __GLASGOW_HASKELL__ < 802
  fromLabel _ = Label
#else
  fromLabel = Label
#endif

-- | Defines a constraint that lets us extract the label from an
-- 'ElField'. Used in 'rmapf' and 'rpuref'.
class (KnownSymbol (Fst a), a ~ '(Fst a, Snd a)) => KnownField a where
instance KnownSymbol l => KnownField (l ::: v) where

-- | Shorthand for working with records of fields as in 'rmapf' and
-- 'rpuref'.
type AllFields fs = (AllConstrained KnownField fs, RecApplicative fs)

-- | Map a function between functors across a 'Rec' taking advantage
-- of knowledge that each element is an 'ElField'.
rmapf :: AllFields fs
      => (forall a. KnownField a => f a -> g a)
      -> Rec f fs -> Rec g fs
rmapf f = (rpureConstrained (Proxy :: Proxy KnownField) (Lift f) <<*>>)

-- | Construct a 'Rec' with 'ElField' elements.
rpuref :: AllFields fs => (forall a. KnownField a => f a) -> Rec f fs
rpuref f = rpureConstrained (Proxy :: Proxy KnownField) f

-- | Operator synonym for 'rmapf'.
(<<$$>>)
  :: AllFields fs
  => (forall a. KnownField a => f a -> g a) -> Rec f fs -> Rec g fs
(<<$$>>) = rmapf

-- | Produce a 'Rec' of the labels of a 'Rec' of 'ElField's.
rlabels :: AllFields fs => Rec (Const String) fs
rlabels = rpuref getLabel'
  where getLabel' :: forall l v. KnownSymbol l
                  => Const String (l ::: v)
        getLabel' = Const (symbolVal (Proxy::Proxy l))

-- * Specializations for working with an 'ARec' of named fields.
