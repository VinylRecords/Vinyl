-- | 'Storable' records offer a very efficient flat, packed
-- representation in memory. In particular, field access is constant
-- time (i.e. it doesn't depend on where in the record the field is)
-- and as fast as possible, but updating fields may not be as
-- efficient. The requirement is that all fields of a record have
-- 'Storable' instances.
--
-- The implementation leaks into the usual vinyl lens API: the
-- requirement of 'Storable' instances necessitates specialization on
-- the functor argument of the record so that GHC can find all
-- required instances at compile time (this is required for
-- constant-time field access). What we do is allow ourselves to write
-- instances of the 'RecElem' and 'RecSubset' classes that provide the
-- main vinyl lens API that are actually restricted to particular
-- choices of the record functor. This is why the 'SRec2' type that
-- implements records here takes two functor arguments: they will
-- usually be the same; we fix one when writing instances and write
-- instance contexts that reference that type, and then require that
-- the methods (e.g. 'rget') are called on records whose functor
-- argument is equal to the one we picked. For usability, we provide
-- an 'SRec' type whose lens API is fixed to 'ElField' as the
-- functor. Other specializations are possible, and the work of those
-- instances can always be passed along to the 'SRec2' functions.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Vinyl.SRec (
  -- * Main record lens API
  SRec(..), toSRec, fromSRec
  -- * Lens API specialized to 'SRec2'
  , sget, sput, slens
  , srecGetSubset, srecSetSubset
  -- * Internals
  , toSRec2, fromSRec2, SRec2(..)
  , sizeOfRec
  , FieldOffset, FieldOffsetAux(..), StorableAt(..)
  , peekField, pokeField
) where
import Data.Monoid (Sum(..))
import Data.Proxy
import Data.Vinyl.Core
import Data.Vinyl.Derived (ElField)
import Data.Vinyl.Functor (Lift(..), Compose(..), type (:.))
import Data.Vinyl.Lens (RecElem(..), RecSubset(..), type (⊆), RecElemFCtx)
import Data.Vinyl.TypeLevel (RImage, RIndex, Nat(..), RecAll, AllConstrained)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtr, ForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

import GHC.IO (IO(IO))
import GHC.Base (realWorld#)
import GHC.TypeLits (Symbol)

-- | A 'Storable'-backed 'Rec'. Each field of such a value has
-- statically known size, allowing for a very efficient representation
-- and very fast field access. The @2@ suffix is due to apparently
-- taking /two/ functor arguments, but the first type parameter is
-- phantom and exists so that we can write multiple instances of
-- 'RecElem' and 'RecSubset' for different functors. It will typically
-- be identical to the second argument. We currently provide instances
-- for the 'ElField' functor; if you wish to use it at a different
-- type, consider using 'sget', 'sput', and 'slens' which work with
-- any functor given that the necessary 'Storable' instances exist.
newtype SRec2 (g :: k -> *) (f :: k -> *) (ts :: [k]) =
  SRec2 (ForeignPtr (Rec f ts))

-- | A simpler type for 'SRec2' whose 'RecElem' and 'RecSubset'
-- instances are specialized to the 'ElField' functor.
newtype SRec f ts = SRecNT { getSRecNT :: SRec2 f f ts }

-- | Create an 'SRec2' from a 'Rec'.
toSRec2 :: Storable (Rec f ts) => Rec f ts -> SRec2 f f ts
toSRec2 x = unsafePerformIO $ do
  ptr <- mallocForeignPtr
  SRec2 ptr <$ (withForeignPtr ptr (flip poke x))
{-# NOINLINE toSRec2 #-}

-- | Create an 'SRec' from a 'Rec'. This should offer very fast field
-- access, but note that its lens API (via 'RecElem' and 'RecSubset')
-- is restricted to the 'ElField' functor.
toSRec :: Storable (Rec f ts) => Rec f ts -> SRec f ts
toSRec = SRecNT . toSRec2
{-# INLINE toSRec #-}

-- | Create a 'Rec' from an 'SRec2'.
fromSRec2 :: Storable (Rec f ts) => SRec2 g f ts -> Rec f ts
fromSRec2 (SRec2 ptr) = inlinePerformIO (withForeignPtr ptr peek)
{-# INLINE fromSRec2 #-}

-- | Create a 'Rec' from an 'SRec'.
fromSRec :: Storable (Rec f ts) => SRec f ts -> Rec f ts
fromSRec (SRecNT s) = fromSRec2 s
{-# INLINE fromSRec #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
-- Copied from the @text@ package
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | Capture a 'Storable' dictionary along with a byte offset from
-- some origin address.
data StorableAt f a where
  StorableAt :: Storable (f a) => {-# UNPACK  #-} !Int -> StorableAt f a

-- | The ability to work with a particular field of a 'Rec' stored at
-- a 'Ptr'.
class (RIndex t ts ~ i, RecAll f ts Storable) => FieldOffsetAux f ts t i where
  -- | Get the byte offset of a field from the given origin and the
  -- 'Storable' dictionary needed to work with that field.
  fieldOffset :: Int -> StorableAt f t

-- | A more concise constraint equivalent to 'FieldOffsetAux'.
class FieldOffsetAux f ts t (RIndex t ts) => FieldOffset f ts t where
instance FieldOffsetAux f ts t (RIndex t ts) => FieldOffset f ts t where

instance (RecAll f (t ': ts) Storable) => FieldOffsetAux f (t ': ts) t 'Z where
  fieldOffset !n = StorableAt n
  {-# INLINE fieldOffset #-}

instance (RIndex t (s ': ts) ~ 'S i,
          FieldOffsetAux f ts t i,
          RecAll f (s ': ts) Storable)
  => FieldOffsetAux f (s ': ts) t ('S i) where
  fieldOffset !n = fieldOffset @f @ts @t @i (n + sizeOf (undefined :: f s))
  {-# INLINE fieldOffset #-}

-- | Set a field in a record stored at a 'ForeignPtr'.
pokeField :: forall f t ts. FieldOffset f ts t
          => ForeignPtr (Rec f ts) -> f t -> IO ()
pokeField fptr x = case fieldOffset @f @ts @t 0 of
                     StorableAt i -> withForeignPtr fptr $ \ptr ->
                                       pokeByteOff ptr i x
{-# INLINE pokeField #-}

-- | Get a field in a record stored at a 'ForeignPtr'.
peekField :: forall f t ts. FieldOffset f ts t
          => ForeignPtr (Rec f ts) -> IO (f t)
peekField fptr = case fieldOffset @f @ts @t 0 of
                   StorableAt i -> withForeignPtr fptr $ \ptr ->
                                     peekByteOff ptr i
{-# INLINE peekField #-}

-- | Get a field from an 'SRec'.
sget :: forall f t ts. FieldOffset f ts t
     => SRec2 f f ts -> f t
sget (SRec2 ptr) = inlinePerformIO (peekField ptr)
{-# INLINE sget #-}

mallocAndCopy :: ForeignPtr a -> Int -> IO (ForeignPtr a)
mallocAndCopy src n = do
  dst <- mallocForeignPtrBytes n
  withForeignPtr src $ \src' ->
    withForeignPtr dst $ \dst' ->
      dst <$ copyBytes dst' src' n

-- | Set a field.
sput :: forall (f :: u -> *) (t :: u) (ts :: [u]).
        ( FieldOffset f ts t
        , RecApplicative ts
        , AllConstrained (FieldOffset f ts) ts)
     => f t -> SRec2 f f ts -> SRec2 f f ts
-- sput x (SRec2 src) = unsafeDupablePerformIO $ do
--   let n = sizeOfRec @u @f @ts @ts
--   dst <- mallocForeignPtrBytes n
--   withForeignPtr src $ \src' ->
--     withForeignPtr dst $ \dst' ->
--       copyBytes dst' src' n
sput !x (SRec2 src) = unsafePerformIO $ do
  let !n = sizeOfRec @u @f @ts @ts
  dst <- mallocAndCopy src n
  SRec2 dst <$ pokeField dst x
{-# INLINE [1] sput #-}

-- pokeFieldUnsafe :: forall f t ts. FieldOffset f ts t
--                 => f t -> SRec2 f f ts -> SRec2 f f ts
-- pokeFieldUnsafe x y@(SRec2 ptr) = unsafeDupablePerformIO (y <$ pokeField ptr x)
-- {-# INLINE [1] pokeFieldUnsafe #-}

{- RULES
"sput" forall x y z. sput x (sput y z) = pokeFieldUnsafe x (sput y z)
"sputUnsafe" forall x y z. sput x (pokeFieldUnsafe y z) = pokeFieldUnsafe x (pokeFieldUnsafe y z)
  #-}

-- | A lens for a field of an 'SRec2'.
slens :: ( Functor g
         , FieldOffset f ts t
         , RecApplicative ts
         , AllConstrained (FieldOffset f ts) ts)
      => (f t -> g (f t)) -> SRec2 f f ts -> g (SRec2 f f ts)
slens f sr = fmap (flip sput sr) (f (sget sr))
{-# INLINE slens #-}

-- Note: we need the functor to appear in the instance head so that we
-- can demand the needed 'Storable' instances. We do this by giving
-- 'SRec2' a phantom tag that duplicates the "real" functor parameter,
-- and define a constraint that the real argument is in fact
-- 'ElField'. This lets us write instances for different applications
-- of @SRec2@ (e.g. instance for @SRec2 Foo@ for records of type
-- @SRec2 Foo Foo ts@, and an instance for @SRec2 Bar@ for records of
-- type @SRec2 Bar Bar ts@).

-- | Field accessors for 'SRec2' specialized to 'ElField' as the
-- functor.
instance ( i ~ RIndex t ts
         , FieldOffset ElField ts t
         , RecApplicative ts
         , AllConstrained (FieldOffset ElField ts) ts)
  => RecElem (SRec2 ElField) t ts i where
  type RecElemFCtx (SRec2 ElField) f = f ~ ElField
  rlens _ = slens
  {-# INLINE rlens #-}
  rget _ = sget
  {-# INLINE rget #-}
  rput = sput
  {-# INLINE rput #-}

instance ( i ~ RIndex (t :: (Symbol,*)) (ts :: [(Symbol,*)])
         , FieldOffset ElField ts t
         , RecApplicative ts
         , AllConstrained (FieldOffset ElField ts) ts)
  => RecElem SRec (t :: (Symbol,*)) (ts :: [(Symbol,*)]) i where
  type RecElemFCtx SRec f = f ~ ElField
  rlens _ f = fmap SRecNT . slens f . getSRecNT
  {-# INLINE rlens #-}
  rget _ = sget . getSRecNT
  {-# INLINE rget #-}
  rput x = SRecNT . sput x . getSRecNT
  {-# INLINE rput #-}

-- | Compute the size in bytes needed to represent a 'Rec' without a
-- 'Storable' instance.
sizeOfRec :: forall (f :: u -> *) (ss :: [u]) (rs :: [u]).
             (AllConstrained (FieldOffset f ss) rs, RecApplicative rs)
          => Int
sizeOfRec = (getSum . rfoldMap go)
              (rpureConstrained prox mkField :: Rec (StorableAt f) rs)
  where prox = Proxy :: Proxy (FieldOffset f ss)
        {-# INLINE prox #-}
        mkField :: forall (t :: u). FieldOffset f ss t => StorableAt f t
        mkField = fieldOffset @f @ss 0
        {-# INLINE mkField #-}
        go :: forall t. StorableAt f t -> Sum Int
        go (StorableAt _) = Sum (sizeOf (undefined :: f t))
        {-# INLINE go #-}
{-# INLINE sizeOfRec #-}

-- | Get a subset of a record's fields.
srecGetSubset :: forall (ss :: [u]) (rs :: [u]) (f :: u -> *).
                 (AllConstrained (FieldOffset f ss) rs,
                  AllConstrained (FieldOffset f rs) rs,
                  RecApplicative rs)
              => SRec2 f f ss -> SRec2 f f rs
srecGetSubset (SRec2 ptr) = unsafeDupablePerformIO $ do
  dst <- mallocForeignPtrBytes
           (sizeOfRec @u @f @ss @rs)
  SRec2 dst <$ (withForeignPtr dst $ \dst' ->
                 rfoldMap unTagIO (Lift . peekNPoke <<$>> peekers <<*>> pokers dst'))
  where peekers :: Rec (IO :. f) rs
        peekers = rpureConstrained (Proxy :: Proxy (FieldOffset f ss)) mkPeeker
        mkPeeker :: FieldOffset f ss t => (IO :. f) t
        mkPeeker = Compose (peekField ptr)
        pokers :: Ptr (Rec f rs) -> Rec (Poker f) rs
        pokers dst = rpureConstrained (Proxy :: Proxy (FieldOffset f rs)) (mkPoker dst)
        mkPoker :: forall t. Ptr (Rec f rs) -> FieldOffset f rs t => Poker f t
        mkPoker dst = case fieldOffset @f @rs @t 0 of
                        StorableAt i -> Lift (TaggedIO . pokeByteOff dst i)
        peekNPoke :: (IO :. f) t -> Poker f t -> TaggedIO t
        peekNPoke (Compose m) (Lift f) = TaggedIO (m >>= unTagIO . f)
{-# INLINABLE srecGetSubset #-}

-- | Phantom tagged 'IO ()' value. Used to work with vinyl's 'Lift'
-- that wants @forall a. f a -> g a@.
newtype TaggedIO a = TaggedIO { unTagIO :: IO () }

-- | A dressed up function of type @f a -> IO ()@
type Poker f = Lift (->) f TaggedIO

-- | Set a subset of a record's fields.
srecSetSubset :: forall (f :: u -> *) (ss :: [u]) (rs :: [u]).
                 (AllConstrained (FieldOffset f ss) rs,
                  RecApplicative rs,
                  rs ⊆ ss,
                  RecAll f ss Storable,
                  AllConstrained (FieldOffset f ss) ss,
                  RecApplicative ss)
              => SRec2 f f ss -> SRec2 f f rs -> SRec2 f f ss
srecSetSubset (SRec2 srcBig) (SRec2 srcSmall) = unsafeDupablePerformIO $ do
  let n = sizeOfRec @u @f @ss @ss
  dst <- mallocForeignPtrBytes n
  withForeignPtr srcBig $ \srcBig' ->
    withForeignPtr dst $ \dst' ->
      copyBytes dst' srcBig' n
  let dic = rcast (reifyConstraintRec @f @ss) :: Rec (Dic f Storable) rs
      smallRec = withStorableDicts dic
                   (withForeignPtr srcSmall $ \srcSmall' -> peek srcSmall')
  SRec2 dst <$ (withForeignPtr dst $ \dst' ->
                 smallRec >>= rfoldMap unTagIO . (pokers dst' <<*>>))
  where pokers :: Ptr (Rec f ss) -> Rec (Poker f) rs
        pokers dst = rpureConstrained (Proxy :: Proxy (FieldOffset f ss))
                                      (mkPoker dst)
        {-# INLINE pokers #-}
        mkPoker :: forall t. FieldOffset f ss t => Ptr (Rec f ss) -> Poker f t
        mkPoker dst = case fieldOffset @f @ss @t 0 of
                        StorableAt i -> Lift (TaggedIO . pokeByteOff dst i)
        {-# INLINE mkPoker #-}
{-# INLINABLE srecSetSubset #-}

data Dic f c a where Dic :: c (f a) => Dic f c a

-- | Use a 'Rec' of dictionaries to satisfy a 'RecAll' constraint.
-- withRecDic :: Rec (Dic f c) ss -> (RecAll f ss c => r) -> r
-- withRecDic RNil x = x
-- withRecDic (Dic :& xs) x = withRecDic xs x

-- | Use a 'Rec' of dictionaries to satisfy a 'Storable' constraint on
-- the underlying 'Rec' itself.
withStorableDicts :: Rec (Dic f Storable) ss -> (Storable (Rec f ss) => r) -> r
withStorableDicts RNil r = r
withStorableDicts (Dic :& xs) r = withStorableDicts xs r

-- | Produce a 'Rec' witness of a 'RecAll' constraint.
reifyConstraintRec :: forall f ss c. (RecAll f ss c, RecApplicative ss)
                   => Rec (Dic f c) ss
reifyConstraintRec = go (rpure Proxy :: Rec Proxy ss)
  where go :: RecAll f ss' c => Rec Proxy ss' -> Rec (Dic f c) ss'
        go RNil = RNil
        go (_ :& xs) = Dic :& go xs
{-# INLINABLE reifyConstraintRec #-}

instance (is ~ RImage rs ss, RecApplicative rs, RecApplicative ss,
          RecSubset Rec rs ss is,
          RecAll ElField ss Storable,
          RecAll ElField rs Storable,
          AllConstrained (FieldOffset ElField rs) rs,
          AllConstrained (FieldOffset ElField ss) ss,
          AllConstrained (FieldOffset ElField ss) rs)
  => RecSubset (SRec2 ElField) rs ss is where
  type RecSubsetFCtx (SRec2 ElField) f = f ~ ElField
  rsubset :: forall g. Functor g
          => (SRec2 ElField ElField rs -> g (SRec2 ElField ElField rs))
          -> SRec2 ElField ElField ss
          -> g (SRec2 ElField ElField ss)
  rsubset f big@(SRec2 _) = fmap (srecSetSubset big) (f smallRec)
    where -- subDict :: Rec (Dic ElField Storable) rs
          -- subDict = rcast (reifyConstraintRec @_ @ss)
          smallRec :: SRec2 ElField ElField rs
          smallRec = -- withRecDic subDict
                                  (srecGetSubset big)
  {-# INLINE rsubset #-}

instance (is ~ RImage rs ss, RecApplicative rs, RecApplicative ss,
         RecSubset Rec rs ss is,
         RecAll ElField ss Storable,
         RecAll ElField rs Storable,
         AllConstrained (FieldOffset ElField rs) rs,
         AllConstrained (FieldOffset ElField ss) ss,
         AllConstrained (FieldOffset ElField ss) rs)
  => RecSubset SRec rs ss is where
  type RecSubsetFCtx SRec f = f ~ ElField
  rsubset f (SRecNT s) = SRecNT <$> rsubset (fmap getSRecNT . f . SRecNT) s
