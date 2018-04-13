{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | Concise vinyl record construction from tuples up to size 8. An
-- example record construction using 'ElField' for named fields:
-- @fieldRec (#x =: True, #y =: 'b') :: FieldRec '[ '("x", Bool), '("y", Char) ]@
module Data.Vinyl.FromTuple where
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor (ElField)
import Data.Vinyl.XRec (XRec(..), IsoXRec(..), HKD)
-- import GHC.TypeLits (TypeError, ErrorMessage(Text))

-- | Convert a tuple of types formed by the application of a common
-- type constructor to a tuple of the common type constructor and a
-- list of the types to which it is applied in the original
-- tuple. E.g. @TupleToRecArgs f (f a, f b) ~ (f, [a,b])@.
type family TupleToRecArgs f t = (r :: (u -> *, [u])) | r -> t where
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z, f g, f h) =
    '(f, [a,b,c,d,e,z,g,h])
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z, f g) = '(f, [a,b,c,d,e,z,g])
  TupleToRecArgs f (f a, f b, f c, f d, f e, f z) = '(f, [a,b,c,d,e,z])
  TupleToRecArgs f (f a, f b, f c, f d, f e) = '(f, [a,b,c,d,e])
  TupleToRecArgs f (f a, f b, f c, f d) = '(f, [a,b,c,d])
  TupleToRecArgs f (f a, f b, f c) = '(f, [a,b,c])
  TupleToRecArgs f (f a, f b) = '(f, [a,b])

-- | Apply the 'Rec' type constructor to a type-level tuple of its
-- arguments.
type family UncurriedRec (t :: (u -> *, [u])) = r | r -> t where
  UncurriedRec '(f, ts) = Rec f ts

-- | Apply the 'XRec' type constructor to a type-level tuple of its
-- arguments.
type family UncurriedXRec (t :: (u -> *, [u])) = r | r -> t where
  UncurriedXRec '(f, ts) = XRec f ts

type family TupleToList t = r | r -> t where
  TupleToList (a,b) = [a,b]
  TupleToList (a,b,c) = [a,b,c]
  TupleToList (a,b,c,d) = [a,b,c,d]
  TupleToList (a,b,c,d,e) = [a,b,c,d,e]
  TupleToList (a,b,c,d,e,f) = [a,b,c,d,e,f]
  TupleToList (a,b,c,d,e,f,g) = [a,b,c,d,e,f,g]
  TupleToList (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h]

type family HKDTuple f t where
  HKDTuple f (a,b) = (HKD f a, HKD f b)
  HKDTuple f (a,b,c) = (HKD f a, HKD f b, HKD f c)
  HKDTuple f (a,b,c,d) = (HKD f a, HKD f b, HKD f c, HKD f d)
  HKDTuple f (a,b,c,d,e) = (HKD f a, HKD f b, HKD f c, HKD f d, HKD f e)
  HKDTuple f (a,b,c,d,e,z) =
    (HKD f a, HKD f b, HKD f c, HKD f d, HKD f e, HKD f z)
  HKDTuple f (a,b,c,d,e,z,g) =
    (HKD f a, HKD f b, HKD f c, HKD f d, HKD f e, HKD f z, HKD f g)
  HKDTuple f (a,b,c,d,e,z,g,h) =
    (HKD f a, HKD f b, HKD f c, HKD f d, HKD f e, HKD f z, HKD f g, HKD f h)

-- | Convert between an 'XRec' and an isomorphic tuple.
class TupleXRec f t where
  -- | Convert an 'XRec' to a tuple. Useful for pattern matching on an
  -- entire record.
  xrecTuple :: XRec f (TupleToList t) -> HKDTuple f t
  -- | Build an 'XRec' from a tuple.
  xrecX :: HKDTuple f t -> XRec f (TupleToList t)

instance TupleXRec f (a,b) where
  xrecTuple (a ::& b ::& XRNil) = (a, b)
  xrecX (a, b) = a ::& b ::& XRNil

instance TupleXRec f (a,b,c) where
  xrecTuple (a ::& b ::& c ::& XRNil) = (a, b, c)
  xrecX (a, b, c) = a ::& b ::& c ::& XRNil

instance TupleXRec f (a,b,c,d) where
  xrecTuple (a ::& b ::& c ::& d ::& XRNil) = (a, b, c, d)
  xrecX (a, b, c, d) = a ::& b ::& c ::& d ::& XRNil

instance TupleXRec f (a,b,c,d,e) where
  xrecTuple (a ::& b ::& c ::& d ::& e ::& XRNil) =
    (a, b, c, d, e)
  xrecX (a, b, c, d, e) = a ::& b ::& c ::& d ::& e ::& XRNil

instance TupleXRec f (a,b,c,d,e,z) where
  xrecTuple (a ::& b ::& c ::& d ::& e ::& z ::& XRNil) =
    (a, b, c, d, e, z)
  xrecX (a, b, c, d, e, z) = a ::& b ::& c ::& d ::& e ::& z ::& XRNil

instance TupleXRec f (a,b,c,d,e,z,g) where
  xrecTuple (a ::& b ::& c ::& d ::& e ::& z ::& g ::& XRNil) =
    (a, b, c, d, e, z, g)
  xrecX (a, b, c, d, e, z, g) = a ::& b ::& c ::& d ::& e ::& z ::& g ::& XRNil

instance TupleXRec f (a,b,c,d,e,z,g,h) where
  xrecTuple (a ::& b ::& c ::& d ::& e ::& z ::& g ::& h ::& XRNil) =
    (a, b, c, d, e, z, g, h)
  xrecX (a, b, c, d, e, z, g, h) = a ::& b ::& c ::& d ::& e ::& z ::& g ::& h ::& XRNil

type family ListToTuple ts = r | r -> ts where
  ListToTuple [a,b] = (a,b)
  ListToTuple [a,b,c] = (a,b,c)
  ListToTuple [a,b,c,d] = (a,b,c,d)
  ListToTuple [a,b,c,d,e] = (a,b,c,d,e)
  ListToTuple [a,b,c,d,e,f] = (a,b,c,d,e,f)
  ListToTuple [a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  ListToTuple [a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  -- ListToTuple x = TypeError (Text "Tuples are only supported up to size 8")

-- The custom type error above breaks type family injectivity :(

-- | Convert a 'Rec' to a tuple going through 'HKD' to reduce
-- syntactic noise. Useful for pattern matching on an entire 'Rec'.
ruple :: ( IsoXRec f ts, TupleXRec f (ListToTuple ts)
         , ts ~ TupleToList (ListToTuple ts) )
      => Rec f ts -> HKDTuple f (ListToTuple ts)
ruple = xrecTuple . toXRec

-- | Build a 'Rec' from a tuple passing through 'XRec'. This admits
-- the most concise syntax for building a 'Rec'. For example, @xrec
-- ("joe", 23) :: Rec Identity '[String, Int]@.
xrec :: (IsoXRec f (TupleToList t), TupleXRec f t)
     => HKDTuple f t -> Rec f (TupleToList t)
xrec = fromXRec . xrecX

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
