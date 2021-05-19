{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Vinyl.TypeLevel (Fst, Snd, RIndex)
import GHC.OverloadedLabels
import GHC.TypeLits

-- | Alias for Field spec
type a ::: b = '(a, b)

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

-- | Get the data payload of an 'ElField'.
getField :: ElField '(s,t) -> t
getField (Field x) = x

-- | Get the label name of an 'ElField'.
getLabel :: forall s t. KnownSymbol s => ElField '(s,t) -> String
getLabel (Field _) = symbolVal (Proxy::Proxy s)

-- | 'ElField' is isomorphic to a functor something like @Compose
-- ElField ('(,) s)@.
fieldMap :: (a -> b) -> ElField '(s,a) -> ElField '(s,b)
fieldMap f (Field x) = Field (f x)
{-# INLINE fieldMap #-}

-- | Something in the spirit of 'traverse' for 'ElField' whose kind
-- fights the standard library.
traverseField :: (KnownSymbol s, Functor f)
              => (a -> b) -> f (ElField '(s,a)) -> ElField '(s, f b)
traverseField f t = Field (fmap (f . getField)  t)

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
  :: forall l f v record us.
     (HasField record l us us v v, RecElemFCtx record f)
  => Label l -> record f us -> f (l ::: v)
rgetf _ = rget @(l ::: v)

-- | Get the value associated with a named field from a record.
rvalf
  :: (HasField record l us us v v, RecElemFCtx record ElField)
  => Label l -> record ElField us -> v
rvalf x = getField . rgetf x

-- | Set a named field. @rputf' #foo 23@ sets the field named @#foo@ to
-- @23@.
rputf' :: forall l v v' record us us'.
          (HasField record l us us' v v', KnownSymbol l, RecElemFCtx record ElField)
       => Label l -> v' -> record ElField us -> record ElField us'
rputf' _ = rput' @_ @(l:::v) . (Field :: v' -> ElField '(l,v'))

-- | Set a named field without changing its type. @rputf #foo 23@ sets
-- the field named @#foo@ to @23@.
rputf :: forall l v record us.
          (HasField record l us us v v, KnownSymbol l, RecElemFCtx record ElField)
       => Label l -> v -> record ElField us -> record ElField us
rputf _ = rput @_ @(l:::v) . Field

-- | A lens into a 'Rec' identified by a 'Label'.
rlensfL' :: forall l v v' record g f us us'.
             (Functor g, HasField record l us us' v v', RecElemFCtx record f)
          => Label l
          -> (f (l ::: v) -> g (f (l ::: v')))
          -> record f us
          -> g (record f us')
rlensfL' _ f = rlens' @(l ::: v) f

-- | A type-preserving lens into a 'Rec' identified by a 'Label'.
rlensfL :: forall l v record g f us.
           (Functor g, HasField record l us us v v, RecElemFCtx record f)
        => Label l
        -> (f (l ::: v) -> g (f (l ::: v)))
        -> record f us
        -> g (record f us)
rlensfL _ f = rlens' @(l ::: v) f

-- | A lens into the payload value of a 'Rec' field identified by a
-- 'Label'.
rlensf' :: forall l v v' record g us us'.
           (Functor g, HasField record l us us' v v', RecElemFCtx record ElField)
        => Label l -> (v -> g v') -> record ElField us -> g (record ElField us')
rlensf' _ f = rlens' @(l ::: v) (rfield f)

-- | A type-preserving lens into the payload value of a 'Rec' field
-- identified by a 'Label'.
rlensf :: forall l v record g us.
          (Functor g, HasField record l us us v v, RecElemFCtx record ElField)
        => Label l -> (v -> g v) -> record ElField us -> g (record ElField us)
rlensf _ f = rlens @(l ::: v) (rfield f)

-- | Shorthand for a 'FieldRec' with a single field.
(=:=) :: KnownSymbol s => Label (s :: Symbol) -> a -> FieldRec '[ '(s,a) ]
(=:=) _ x = Field x :& RNil

-- | A proxy for field types.
data SField (field :: k) = SField

instance Eq (SField a) where _ == _ = True
instance Ord (SField a) where compare _ _ = EQ
instance KnownSymbol s => Show (SField '(s,t)) where
  show _ = "SField "++symbolVal (Proxy::Proxy s)

type family FieldType l fs where
  FieldType l '[] = TypeError ('Text "Cannot find label "
                               ':<>: 'ShowType l
                               ':<>: 'Text " in fields")
  FieldType l ((l ::: v) ': fs) = v
  FieldType l ((l' ::: v') ': fs) = FieldType l fs

-- | Constraint that a label is associated with a particular type in a
-- record.
type HasField record l fs fs' v v' =
  (RecElem record (l ::: v) (l ::: v') fs fs' (RIndex (l ::: v) fs), FieldType l fs ~ v, FieldType l fs' ~ v')

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
type AllFields fs = (RPureConstrained KnownField fs, RecApplicative fs, RApply fs)

-- | Map a function between functors across a 'Rec' taking advantage
-- of knowledge that each element is an 'ElField'.
rmapf :: AllFields fs
      => (forall a. KnownField a => f a -> g a)
      -> Rec f fs -> Rec g fs
rmapf f = (rpureConstrained @KnownField (Lift f) <<*>>)

-- | Remove the first component (e.g. the label) from a type-level
-- list of pairs.
type family Unlabeled ts where
  Unlabeled '[] = '[]
  Unlabeled ('(s,x) ': xs) = x ': Unlabeled xs

-- | Facilities for removing and replacing the type-level label, or
-- column name, part of a record.
class StripFieldNames ts where
  stripNames :: Rec ElField ts -> Rec Identity (Unlabeled ts)
  stripNames' :: Functor f => Rec (f :. ElField) ts -> Rec f (Unlabeled ts)
  withNames :: Rec Identity (Unlabeled ts) -> Rec ElField ts
  withNames' :: Functor f => Rec f (Unlabeled ts) -> Rec (f :. ElField) ts

instance StripFieldNames '[] where
  stripNames RNil = RNil
  stripNames' RNil = RNil
  withNames RNil = RNil
  withNames' RNil = RNil

instance (KnownSymbol s, StripFieldNames ts) => StripFieldNames ('(s,t) ': ts) where
  stripNames (Field x :& xs) = pure x :& stripNames xs
  stripNames' (Compose x :& xs) = fmap getField x :& stripNames' xs
  withNames (Identity x :& xs) = Field x :& withNames xs
  withNames' (x :& xs) = Compose (fmap Field x) :& withNames' xs

-- | Construct a 'Rec' with 'ElField' elements.
rpuref :: AllFields fs => (forall a. KnownField a => f a) -> Rec f fs
rpuref f = rpureConstrained @KnownField f

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
