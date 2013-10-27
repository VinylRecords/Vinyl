{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields.
module Data.Vinyl.MiniLens where
import Control.Applicative
import Control.Monad.Identity
import Data.Vinyl.Field
import Data.Vinyl.Rec
import Data.Vinyl.Witnesses

-- | Project a field from a 'Rec'.
getField' :: IElem (sy ::: t) rs => (sy ::: t) -> Rec rs f -> f t
getField' r = getConst . miniLensField' r Const
{-# INLINE getField' #-}

-- | Project a field from a 'PlainRec'.
getField :: IElem (sy ::: t) rs => (sy ::: t) -> PlainRec rs -> t
getField = (runIdentity .) . getField'
{-# INLINE getField #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
setField' :: IElem (sy ::: t) rs => (sy ::: t) -> f t -> Rec rs f -> Rec rs f
setField' r x = runIdentity . miniLensField' r (Identity . const x)
{-# INLINE setField' #-}

-- | Set a field in a 'PlainRec'.
setField :: IElem (sy:::t) rs => (sy:::t) -> t -> PlainRec rs -> PlainRec rs
setField r x = setField' r (Identity x)
{-# INLINE setField #-}

-- | Modify a field.
modField :: (IElem (sy:::t) rs, Functor f)
         => (sy:::t) -> (t -> t) -> Rec rs f -> Rec rs f
modField r f = runIdentity . miniLensField' r (Identity . fmap f)
{-# INLINE modField #-}

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update.
miniLensField' :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
               => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
miniLensField' _ f = go implicitly
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
        go' (There p) (x :& xs) = fmap (x :&) (go' p xs)
        {-# INLINABLE go' #-}
{-# INLINE miniLensField' #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not supported.
miniLensField :: forall r rs sy t g. (r ~ (sy:::t), IElem r rs, Functor g)
               => r -> (t -> g t) -> PlainRec rs -> g (PlainRec rs)
miniLensField r = miniLensField' r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE miniLensField #-}
