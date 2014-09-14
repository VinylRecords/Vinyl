{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vinyl.Core where

import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Vinyl.Functor
import Control.Applicative hiding (Const(..))
import Data.Typeable (Proxy(..))
import Data.List (intercalate)
import Data.Vinyl.TypeLevel

-- | A record is parameterized by a universe @u@, list of rows @rs@, and an
-- interpretation @f@.
data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)
infixr :&

instance Monoid (Rec f '[]) where
  mempty = RNil
  RNil `mappend` RNil = RNil

instance (Monoid (f r), Monoid (Rec f rs)) => Monoid (Rec f (r ': rs)) where
  mempty = mempty :& mempty
  (x :& xs) `mappend` (y :& ys) = (x <> y) :& (xs <> ys)

instance Eq (Rec f '[]) where
  _ == _ = True
instance (Eq (f r), Eq (Rec f rs)) => Eq (Rec f (r ': rs)) where
  (x :& xs) == (y :& ys) = (x == y) && (xs == ys)

infixr 5  <+>
infixl 8 <<$>>
infixl 8 <<*>>

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | Append for records.
rappend
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
rappend RNil ys = ys
rappend (x :& xs) ys = x :& (xs `rappend` ys)

(<+>)
  :: Rec f as
  -> Rec f bs
  -> Rec f (as ++ bs)
(<+>) = rappend

rmap
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
rmap _ RNil = RNil
rmap η (x :& xs) = η x :& (η `rmap` xs)
{-# INLINE rmap #-}

(<<$>>)
  :: (forall x. f x -> g x)
  -> Rec f rs
  -> Rec g rs
(<<$>>) = rmap
{-# INLINE (<<$>>) #-}

(<<&>>)
  :: Rec f rs
  -> (forall x. f x -> g x)
  -> Rec g rs
xs <<&>> f = rmap f xs
{-# INLINE (<<&>>) #-}

rapply
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
rapply RNil RNil = RNil
rapply (f :& fs) (x :& xs) = getLift f x :& (fs `rapply` xs)
{-# INLINE rapply #-}

(<<*>>)
  :: Rec (Lift (->) f g) rs
  -> Rec f rs
  -> Rec g rs
(<<*>>) = rapply
{-# INLINE (<<*>>) #-}

class RecApplicative rs where
  rpure
    :: (forall x. f x)
    -> Rec f rs
instance RecApplicative '[] where
  rpure _ = RNil
  {-# INLINE rpure #-}
instance RecApplicative rs => RecApplicative (r ': rs) where
  rpure s = s :& rpure s
  {-# INLINE rpure #-}

rtraverse
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs

recordToList
  :: Rec (Const a) rs
  -> [a]
recordToList RNil = []
recordToList (x :& xs) = getConst x : recordToList xs

-- | Wrap up a value with a capability given by its type
data Dict c a where
  Dict
    :: c a
    => a
    -> Dict c a

-- | Sometimes we may know something for all fields of a record, but when you
-- try to use that judgement upon a particular field, you run into trouble. In
-- this case, it is necessary to reify the proof into each field.
reifyConstraint
  :: RecAll f rs c
  => proxy c
  -> Rec f rs
  -> Rec (Dict c :. f) rs
reifyConstraint prx rec =
  case rec of
    RNil -> RNil
    (x :& xs) -> Compose (Dict x) :& reifyConstraint prx xs

instance RecAll f rs Show => Show (Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . intercalate "; "
      . recordToList
      . rmap (\(Compose (Dict x)) -> Const $ show x)
      $ reifyConstraint (Proxy :: Proxy Show) xs

instance Storable (Rec f '[]) where
  sizeOf _    = 0
  alignment _ = 0
  peek _      = return RNil
  poke _ RNil = return ()

instance (Storable (f r), Storable (Rec f rs)) => Storable (Rec f (r ': rs)) where
  sizeOf _ = sizeOf (undefined :: f r) + sizeOf (undefined :: Rec f rs)
  {-# INLINABLE sizeOf #-}
  alignment _ =  alignment (undefined :: f r)
  {-# INLINABLE alignment #-}
  peek ptr = do !x <- peek (castPtr ptr)
                !xs <- peek (ptr `plusPtr` sizeOf (undefined :: f r))
                return $ x :& xs
  {-# INLINABLE peek #-}
  poke ptr (!x :& xs) = poke (castPtr ptr) x >> poke (ptr `plusPtr` sizeOf (undefined :: f r)) xs
  {-# INLINEABLE poke #-}
