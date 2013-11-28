{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vinyl.LazyLens where

import Control.Applicative
import Data.Functor.Identity
import Data.Vinyl.Field
import Data.Vinyl.LazyRec
import Data.Vinyl.Witnesses

-- Because LazyRec doesn't carry a constraint that (y ~ (sy ::: t)), we 
-- need to carry it around in the witness.
data Elem' :: * -> [*] -> * where
  Here' :: Elem' x (x ': xs)
  There' :: (y ~ (sy ::: t)) => Elem' x xs -> Elem' x (y ': xs)

instance Implicit (Elem' x (x ': xs)) where
  implicitly = Here'
instance (y ~ (sy ::: t), Implicit (Elem' x xs)) => Implicit (Elem' x (y ': xs)) where
  implicitly = There' implicitly

type IElem' x xs = Implicit (Elem' x xs)

-- | Project a field from a 'Rec'.
rGetL' :: IElem' (sy ::: t) rs => (sy ::: t) -> LazyRec rs f -> f t
rGetL' r = getConst . rLensL' r Const
{-# INLINE rGetL' #-}

-- | Project a field from a 'PlainRec'.
rGetL :: IElem' (sy ::: t) rs => (sy ::: t) -> LazyRec rs Identity -> t
rGetL = (runIdentity .) . rGetL'
{-# INLINE rGetL #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
rPutL' :: IElem' (sy ::: t) rs => (sy ::: t) -> f t -> LazyRec rs f -> LazyRec rs f
rPutL' r x = runIdentity . rLensL' r (Identity . const x)
{-# INLINE rPutL' #-}

-- | Set a field in a 'PlainRec'.
rPutL :: IElem' (sy:::t) rs => (sy:::t) -> t -> LazyRec rs Identity -> LazyRec rs Identity
rPutL r x = rPutL' r (Identity x)
{-# INLINE rPutL #-}

-- | Modify a field.
rModL :: (IElem' (sy:::t) rs, Functor f)
     => (sy:::t) -> (t -> t) -> LazyRec rs f -> LazyRec rs f
rModL r f = runIdentity . rLensL' r (Identity . fmap f)
{-# INLINE rModL #-}

-- We manually unroll several levels of 'Elem' value traversal to help
-- GHC statically index into small records.

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLensL' :: IElem' (sy:::t) rs => (sy:::t) -> Lens' (LazyRec rs f) (f t)
rLensL' :: forall r rs sy t f g. (r ~ (sy:::t), Implicit (Elem' r rs), Functor g)
       => r -> (f t -> g (f t)) -> LazyRec rs f -> g (LazyRec rs f)
rLensL' _ f = go implicitly
  where 
    go :: Elem' r rr -> LazyRec rr f -> g (LazyRec rr f)
    go Here' ~(x :&~ xs) = fmap (:&~ xs) (f x)
    go (There' Here') ~(a :&~ x :&~ xs) = fmap ((a :&~) . (:&~ xs)) (f x)
    go (There' (There' Here')) ~(a :&~ b :&~ x :&~ xs) =
      fmap (\x' -> a :&~ b :&~ x' :&~ xs) (f x)
    go (There' (There' (There' Here'))) ~(a :&~ b :&~ c :&~ x :&~ xs) =
      fmap (\x' -> a :&~ b :&~ c :&~ x' :&~ xs) (f x)
    go (There' (There' (There' (There' Here')))) ~(a :&~ b :&~ c :&~ d :&~ x :&~ xs) =
      fmap (\x' -> a :&~ b :&~ c :&~ d :&~ x' :&~ xs) (f x)
    go (There' (There' (There' (There' p)))) ~(a :&~ b :&~ c :&~ d :&~ xs) =
      fmap (\xs' -> a :&~ b :&~ c :&~ d :&~ xs') (go' p xs)
    {-# INLINE go #-}

    go' :: Elem' r rr -> LazyRec rr f -> g (LazyRec rr f)
    go' Here' ~(x :&~ xs) = fmap (:&~ xs) (f x)
    go' (There' p) ~(x :&~ xs) = fmap (x :&~) (go p xs)
    {-# INLINABLE go' #-}
{-# INLINE rLensL' #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not
-- supported. In the parlance of the @lens@ package,
-- 
-- > rLensL :: IElem' (sy:::t) rs => (sy:::t) -> Lens' (LazyRec rs Identity) t
rLensL :: forall r rs sy t g. (r ~ (sy:::t), IElem' r rs, Functor g)
      => r -> (t -> g t) -> LazyRec rs Identity -> g (LazyRec rs Identity)
rLensL r = rLensL' r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE rLensL #-}
