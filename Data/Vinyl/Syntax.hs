{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses,
             PatternSynonyms, PolyKinds, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeFamilyDependencies
             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Concise Vinyl record construction syntax. This module exports an
-- orphan instance to make working with labels simpler. It will
-- conflict with other libraries that provide functional-looking
-- syntax for labels (i.e. placing a label in function application
-- position, as in @#age 23@).
--
-- Example:
-- @fieldRec (#x True, #y 'b') :: FieldRec '[ '("x", Bool), '("y", Char) ]@
module Data.Vinyl.Syntax where
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (ElField(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol)

instance forall s a b. (KnownSymbol s, b ~ ElField '(s,a))
  => IsLabel s (a -> b) where
#if __GLASGOW_HASKELL__ < 802
  fromLabel _ = Field @s @a
#else
  fromLabel = Field @s @a
#endif

type family TupleToRecArgs f t = (r :: (u -> *, [u])) | r -> t where
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z, f g, f h) =
    '(f, [a,b,c,d,e,z,g,h])
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z, f g) = '(f, [a,b,c,d,e,z,g])
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z) = '(f, [a,b,c,d,e,z])
  TupleToRecArgs f (f a, f b, f c, f d, f e) = '(f, [a,b,c,d,e])
  TupleToRecArgs f (f a, f b, f c, f d) = '(f, [a,b,c,d])
  TupleToRecArgs f (f a, f b, f c) = '(f, [a,b,c])
  TupleToRecArgs f (f a, f b) = '(f, [a,b])

type family UncurriedRec (t :: (u -> *, [u])) = r | r -> t where
  UncurriedRec '(f, ts) = Rec f ts

-- | Build a 'Rec' from a tuple. An example would be building a value
-- of type @Rec f '[a,b]@ from a tuple of values with type @'(f a, f
-- b)@.
class TupleRec f t where
  record :: t -> UncurriedRec (TupleToRecArgs f t)

instance TupleRec f (f a, f b) where
  record (a,b) = a :& b :& RNil

instance TupleRec f (f a, f b, f c) where
  record (a,b,c) = a :& b :& c :& RNil

instance TupleRec f (f a, f b, f c, f d) where
  record (a,b,c,d) = a :& b :& c :& d :& RNil

instance TupleRec f (f a, f b, f c, f d, f e) where
  record (a,b,c,d,e) = a :& b :& c :& d :& e :& RNil

instance TupleRec f (f a, f b, f c, f d, f e, f z) where
  record (a,b,c,d,e,z) = a :& b :& c :& d :& e :& z :& RNil

instance TupleRec f (f a, f b, f c, f d, f e, f z, f g) where
  record (a,b,c,d,e,z,g) = a :& b :& c :& d :& e :& z :& g :& RNil

instance TupleRec f (f a, f b, f c, f d, f e, f z, f g, f h) where
  record (a,b,c,d,e,z,g,h) = a :& b :& c :& d :& e :& z :& g :& h :& RNil

-- | Build a 'FieldRec' from a tuple of 'ElField' values.
fieldRec :: TupleRec ElField t => t -> UncurriedRec (TupleToRecArgs ElField t)
fieldRec = record @ElField
