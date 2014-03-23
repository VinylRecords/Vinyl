{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields. Lenses produced with 'rLens' are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens where
import Control.Applicative
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Field
import Data.Vinyl.Rec
import Data.Vinyl.Witnesses

-- | Project a field from a 'Rec'.
rGet' :: IElem (sy ::: t) rs => (sy ::: t) -> Rec rs f -> f t
rGet' r = getConst . rLens' r Const
{-# INLINE rGet' #-}

-- | Project a field from a 'PlainRec'.
rGet :: IElem (sy ::: t) rs => (sy ::: t) -> PlainRec rs -> t
rGet = (runIdentity .) . rGet'
{-# INLINE rGet #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
rPut' :: IElem (sy ::: t) rs => (sy ::: t) -> f t -> Rec rs f -> Rec rs f
rPut' r x = runIdentity . rLens' r (Identity . const x)
{-# INLINE rPut' #-}

-- | Set a field in a 'PlainRec'.
rPut :: IElem (sy:::t) rs => (sy:::t) -> t -> PlainRec rs -> PlainRec rs
rPut r x = rPut' r (Identity x)
{-# INLINE rPut #-}

-- | Modify a field.
rMod :: (IElem (sy:::t) rs, Functor f)
     => (sy:::t) -> (t -> t) -> Rec rs f -> Rec rs f
rMod r f = runIdentity . rLens' r (Identity . fmap f)
{-# INLINE rMod #-}

-- We manually unroll several levels of 'Elem' value traversal to help
-- GHC statically index into small records.

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLens' :: IElem (sy:::t) rs => (sy:::t) -> Lens' (Rec rs f) (f t)
rLens' :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
rLens' _ f = go implicitly
  where go :: Elem r rr -> Rec rr f -> g (Rec rr f)
        go Here (x :& xs) = fmap (:& xs) (f x)
        go (There Here) (a :& x :& xs) = fmap ((a :&) . (:& xs)) (f x)
        go (There (There Here)) (a :& b :& x :& xs) =
          fmap (\x' -> a :& b :& x' :& xs) (f x)
        go (There (There (There Here))) (a :& b :& c :& x :& xs) =
          fmap (\x' -> a :& b :& c :& x' :& xs) (f x)
        go (There (There (There (There Here)))) (a :& b :& c :& d :& x :& xs) =
          fmap (\x' -> a :& b :& c :& d :& x' :& xs) (f x)
        go (There (There (There (There p)))) (a :& b :& c :& d :& xs) =
          fmap (\xs' -> a :& b :& c :& d :& xs') (go' p xs)
        {-# INLINE go #-}

        go' :: Elem r rr -> Rec rr f -> g (Rec rr f)
        go' Here (x :& xs) = fmap (:& xs) (f x)
        go' (There p) (x :& xs) = fmap (x :&) (go p xs)
        {-# INLINABLE go' #-}
{-# INLINE rLens' #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not
-- supported. In the parlance of the @lens@ package,
--
-- > rLens :: IElem (sy:::t) rs => (sy:::t) -> Lens' (PlainRec rs) t
rLens :: forall r rs sy t g. (r ~ (sy:::t), IElem r rs, Functor g)
      => r -> (t -> g t) -> PlainRec rs -> g (PlainRec rs)
rLens r = rLens' r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE rLens #-}
