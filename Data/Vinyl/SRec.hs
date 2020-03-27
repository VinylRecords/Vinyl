-- | 'Storable' records offer an efficient flat, packed representation
-- in memory. In particular, field access is constant time (i.e. it
-- doesn't depend on where in the record the field is) and as fast as
-- possible, but updating fields may not be as efficient. The
-- requirement is that all fields of a record have 'Storable'
-- instances.
--
-- The implementation leaks into the usual vinyl lens API: the
-- requirement of 'Storable' instances necessitates specialization on
-- the functor argument of the record so that GHC can find all
-- required instances at compile time (this is required for
-- constant-time field access). What we do is allow ourselves to write
-- instances of the 'RecElem' and 'RecSubset' classes (that provide
-- the main vinyl lens API) that are restricted to particular choices
-- of the record functor. This is why the 'SRec2' type that implements
-- records here takes two functor arguments: they will usually be the
-- same; we fix one when writing instances and write instance contexts
-- that reference that type, and then require that the methods
-- (e.g. 'rget') are called on records whose functor argument is equal
-- to the one we picked. For usability, we provide an 'SRec' type
-- whose lens API is fixed to 'ElField' as the functor. Other
-- specializations are possible, and the work of those instances can
-- always be passed along to the 'SRec2' functions.
--
-- Note that the lens field accessors for 'SRec' do not support
-- changing the types of the fields as they do for 'Rec' and
-- 'ARec'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- We get warnings about incomplete patterns on various class
-- instances.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Data.Vinyl.SRec (
  -- * Main record lens API
  SRec(..), toSRec, fromSRec
  -- * Lens API specialized to 'SRec2'
  , sget, sput, slens
  , srecGetSubset, srecSetSubset
  -- * Internals
  , toSRec2, fromSRec2, SRec2(..)
  , FieldOffset, FieldOffsetAux(..), StorableAt(..)
  , peekField, pokeField
) where
import Data.Coerce (coerce)
import Data.Vinyl.Core
import Data.Vinyl.Functor (Lift(..), Compose(..), type (:.), ElField)
import Data.Vinyl.Lens (RecElem(..), RecSubset(..), type (⊆), RecElemFCtx)
import Data.Vinyl.TypeLevel (NatToInt, RImage, RIndex, Nat(..), RecAll, AllConstrained)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

import GHC.IO (IO(IO))
import GHC.Base (realWorld#)
import GHC.TypeLits (Symbol)

import GHC.Prim (MutableByteArray#, newAlignedPinnedByteArray#, byteArrayContents#)
import GHC.Prim (unsafeCoerce#, touch#, RealWorld)
import GHC.Ptr (Ptr(..))
import GHC.Types (Int(..))

-- * Byte array code adapted from the `memory` package.

data Bytes = Bytes (MutableByteArray# RealWorld)

newBytes :: Int -> IO Bytes
newBytes (I# n) = IO $ \s ->
  case newAlignedPinnedByteArray# n 8# s of
    (# s', mbarr #) -> (# s', Bytes mbarr #)

touchBytes :: Bytes -> IO ()
touchBytes (Bytes mbarr) = IO $ \s -> case touch# mbarr s of s' -> (# s', () #)
{-# INLINE touchBytes #-}

withBytesPtr :: Bytes -> (Ptr a -> IO r) -> IO r
withBytesPtr b@(Bytes mbarr) f = do
  f (Ptr (byteArrayContents# (unsafeCoerce# mbarr))) <* touchBytes b
{-# INLINE withBytesPtr #-}

-- * Pun ForeignPtr names to ease refactoring

newtype ForeignPtr (a :: k) = ForeignPtr Bytes

withForeignPtr :: ForeignPtr a -> (Ptr b -> IO r) -> IO r
withForeignPtr (ForeignPtr b) = withBytesPtr b
{-# INLINE withForeignPtr #-}

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes = fmap ForeignPtr . newBytes
{-# INLINE mallocForeignPtrBytes #-}

-- * The SRec types

-- | A 'Storable'-backed 'Rec'. Each field of such a value has
-- statically known size, allowing for a very efficient representation
-- and very fast field access. The @2@ suffix is due to apparently
-- taking /two/ functor arguments, but the first type parameter is
-- phantom and exists so that we can write multiple instances of
-- 'RecElem' and 'RecSubset' for different functors. The first functor
-- argument will typically be identical to the second argument. We
-- currently provide instances for the 'ElField' functor; if you wish
-- to use it at a different type, consider using 'sget', 'sput', and
-- 'slens' which work with any functor given that the necessary
-- 'Storable' instances exist.
newtype SRec2 (g :: k -> *) (f :: k -> *) (ts :: [k]) =
  SRec2 (ForeignPtr (Rec f ts))

-- | A simpler type for 'SRec2' whose 'RecElem' and 'RecSubset'
-- instances are specialized to the 'ElField' functor.
newtype SRec f ts = SRecNT { getSRecNT :: SRec2 f f ts }

-- | Create an 'SRec2' from a 'Rec'.
toSRec2 :: forall f ts. Storable (Rec f ts) => Rec f ts -> SRec2 f f ts
toSRec2 x = unsafePerformIO $ do
  ptr <- mallocForeignPtrBytes (sizeOf (undefined :: Rec f ts))
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
#if __GLASGOW_HASKELL__ < 810
sput :: forall (f :: u -> *) (t :: u) (ts :: [u]).
#else
sput :: forall u (f :: u -> *) (t :: u) (ts :: [u]).
#endif
        ( FieldOffset f ts t
        , Storable (Rec f ts)
        , AllConstrained (FieldOffset f ts) ts)
     => f t -> SRec2 f f ts -> SRec2 f f ts
sput !x (SRec2 src) = unsafePerformIO $ do
  let !n = sizeOf (undefined :: Rec f ts)
  dst <- mallocAndCopy src n
  SRec2 dst <$ pokeField dst x
{-# INLINE [1] sput #-}

pokeFieldUnsafe :: forall f t ts. FieldOffset f ts t
                => f t -> SRec2 f f ts -> SRec2 f f ts
pokeFieldUnsafe x y@(SRec2 ptr) = unsafeDupablePerformIO (y <$ pokeField ptr x)
{-# INLINE [1] pokeFieldUnsafe #-}

{-# RULES
"sput" forall x y z. sput x (sput y z) = pokeFieldUnsafe x (sput y z)
"sputUnsafe" forall x y z. sput x (pokeFieldUnsafe y z) = pokeFieldUnsafe x (pokeFieldUnsafe y z)
  #-}

-- | A lens for a field of an 'SRec2'.
slens :: ( Functor g
         , FieldOffset f ts t
         , Storable (Rec f ts)
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
         , NatToInt i
         , FieldOffset ElField ts t
         , Storable (Rec ElField ts)
         , AllConstrained (FieldOffset ElField ts) ts)
  => RecElem (SRec2 ElField) t t ts ts i where
  type RecElemFCtx (SRec2 ElField) f = f ~ ElField
  rlensC = slens
  {-# INLINE rlensC #-}
  rgetC = sget
  {-# INLINE rgetC #-}
  rputC = sput
  {-# INLINE rputC #-}


coerceSRec1to2 :: SRec f ts -> SRec2 f f ts
coerceSRec1to2 = coerce

coerceSRec2to1 :: SRec2 f f ts -> SRec f ts
coerceSRec2to1 = coerce

instance ( i ~ RIndex (t :: (Symbol,*)) (ts :: [(Symbol,*)])
         , NatToInt i
         , FieldOffset ElField ts t
         , Storable (Rec ElField ts)
         , AllConstrained (FieldOffset ElField ts) ts)
  => RecElem SRec (t :: (Symbol,*)) t (ts :: [(Symbol,*)]) ts i where
  type RecElemFCtx SRec f = f ~ ElField
  rlensC f = fmap coerceSRec2to1 . slens f . coerceSRec1to2
  {-# INLINE rlensC #-}
  rgetC = sget . coerceSRec1to2
  {-# INLINE rgetC #-}
  rputC x = coerceSRec2to1 . sput x . coerceSRec1to2
  {-# INLINE rputC #-}

-- | Get a subset of a record's fields.
#if __GLASGOW_HASKELL__ < 810
srecGetSubset :: forall (ss :: [u]) (rs :: [u]) (f :: u -> *).
#else
srecGetSubset :: forall u (ss :: [u]) (rs :: [u]) (f :: u -> *).
#endif
                 (RPureConstrained (FieldOffset f ss) rs,
                  RPureConstrained (FieldOffset f rs) rs,
                  RFoldMap rs, RMap rs, RApply rs,
                  Storable (Rec f rs))
              => SRec2 f f ss -> SRec2 f f rs
srecGetSubset (SRec2 ptr) = unsafeDupablePerformIO $ do
  dst <- mallocForeignPtrBytes (sizeOf (undefined :: Rec f rs))
  SRec2 dst <$ (withForeignPtr dst $ \dst' ->
                 rfoldMap @rs unTagIO (peekSmallPokeBig dst'))
  where peekers :: Rec (IO :. f) rs
        peekers = rpureConstrained @(FieldOffset f ss) mkPeeker
        {-# INLINE peekers #-}
        mkPeeker :: FieldOffset f ss t => (IO :. f) t
        mkPeeker = Compose (peekField ptr)
        {-# INLINE mkPeeker #-}
        pokers :: Ptr (Rec f rs) -> Rec (Poker f) rs
        pokers dst = rpureConstrained @(FieldOffset f rs) (mkPoker dst)
        {-# INLINE pokers #-}
        mkPoker :: forall t. Ptr (Rec f rs) -> FieldOffset f rs t => Poker f t
        mkPoker dst = case fieldOffset @f @rs @t 0 of
                        StorableAt i -> Lift (TaggedIO . pokeByteOff dst i)
        {-# INLINE mkPoker #-}
        peekNPoke :: (IO :. f) t -> Poker f t -> TaggedIO t
        peekNPoke (Compose m) (Lift f) = TaggedIO (m >>= unTagIO . f)
        {-# INLINE peekNPoke #-}
        peekSmallPokeBig :: Ptr (Rec f rs) -> Rec TaggedIO rs
        peekSmallPokeBig dst' = Lift . peekNPoke <<$>> peekers <<*>> pokers dst'
{-# INLINE srecGetSubset #-}

-- | Phantom tagged 'IO ()' value. Used to work with vinyl's 'Lift'
-- that wants @forall a. f a -> g a@.
newtype TaggedIO a = TaggedIO { unTagIO :: IO () }

-- | A dressed up function of type @f a -> IO ()@
type Poker f = Lift (->) f TaggedIO

-- | Set a subset of a record's fields.
#if __GLASGOW_HASKELL__ < 810
srecSetSubset :: forall (f :: u -> *) (ss :: [u]) (rs :: [u]).
#else
srecSetSubset :: forall u (f :: u -> *) (ss :: [u]) (rs :: [u]).
#endif
                 (rs ⊆ ss,
                  RPureConstrained (FieldOffset f ss) rs,
                  RPureConstrained (FieldOffset f rs) rs,
                  RFoldMap rs, RMap rs, RApply rs,
                  Storable (Rec f ss))
              => SRec2 f f ss -> SRec2 f f rs -> SRec2 f f ss
srecSetSubset (SRec2 srcBig) (SRec2 srcSmall) = unsafeDupablePerformIO $ do
  let n = sizeOf (undefined :: Rec f ss)
  dst <- mallocForeignPtrBytes n
  withForeignPtr srcBig $ \srcBig' ->
    withForeignPtr dst $ \dst' ->
      copyBytes dst' srcBig' n
  SRec2 dst <$ (withForeignPtr dst $ \dst' ->
                 rfoldMap @rs unTagIO
                           (Lift . peekNPoke <<$>> peekers <<*>> pokers dst'))
  where peekers :: Rec (IO :. f) rs
        peekers = rpureConstrained @(FieldOffset f rs) mkPeeker
        {-# INLINE peekers #-}
        mkPeeker :: FieldOffset f rs t => (IO :. f) t
        mkPeeker = Compose (peekField srcSmall)

        pokers :: Ptr (Rec f ss) -> Rec (Poker f) rs
        pokers dst = rpureConstrained @(FieldOffset f ss) (mkPoker dst)
        {-# INLINE pokers #-}
        mkPoker :: forall t. FieldOffset f ss t => Ptr (Rec f ss) -> Poker f t
        mkPoker dst = case fieldOffset @f @ss @t 0 of
                        StorableAt i -> Lift (TaggedIO . pokeByteOff dst i)
        {-# INLINE mkPoker #-}
        peekNPoke :: (IO :. f) t -> Poker f t -> TaggedIO t
        peekNPoke (Compose m) (Lift f) = TaggedIO (m >>= unTagIO . f)
        {-# INLINE peekNPoke #-}
{-# INLINE srecSetSubset #-}

instance (is ~ RImage rs ss,
          RecSubset Rec rs ss is,
          Storable (Rec ElField rs),
          Storable (Rec ElField ss),
          RPureConstrained (FieldOffset ElField ss) rs,
          RPureConstrained (FieldOffset ElField rs) rs,
          RFoldMap rs, RMap rs, RApply rs)
  => RecSubset (SRec2 ElField) rs ss is where
  type RecSubsetFCtx (SRec2 ElField) f = f ~ ElField
  rsubsetC :: forall g. Functor g
           => (SRec2 ElField ElField rs -> g (SRec2 ElField ElField rs))
           -> SRec2 ElField ElField ss
           -> g (SRec2 ElField ElField ss)
  rsubsetC f big@(SRec2 _) = fmap (srecSetSubset big) (f smallRec)
    where smallRec :: SRec2 ElField ElField rs
          smallRec = srecGetSubset big
          {-# INLINE smallRec #-}
  {-# INLINE rsubsetC #-}

instance (is ~ RImage rs ss,
          RecSubset Rec rs ss is,
          Storable (Rec ElField rs),
          Storable (Rec ElField ss),
          RPureConstrained (FieldOffset ElField ss) rs,
          RPureConstrained (FieldOffset ElField rs) rs,
          RFoldMap rs, RMap rs, RApply rs)
  => RecSubset SRec rs ss is where
  type RecSubsetFCtx SRec f = f ~ ElField
  rsubsetC f (SRecNT s) = SRecNT <$> rsubsetC (fmap getSRecNT . f . SRecNT) s
  {-# INLINE rsubsetC #-}
