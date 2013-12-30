{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
-- | A small, /en passant/ lens implementation to provide accessors
-- for record rLens's. Lenses produced with 'rLens' are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens 
    ( rLens', rLens
    , rGet', rGet
    , rPut', rPut
    , rMod
    , subset
    ) where
import Control.Applicative
import Data.Functor.Identity
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

-- | Modify a field
rMod :: (IElem (sy:::t) rs, Functor f)
     => (sy:::t) -> (t -> t) -> Rec rs f -> Rec rs f
rMod r f = runIdentity . rLens' r (Identity . fmap f)
{-# INLINE rMod #-}

-- The unusual implementaiton of rLens' below is based on 
-- http://unlines.wordpress.com/2009/11/05/tricking-ghc-into-evaluating-recursive-functions-at-compile-time/
-- and should result in GHC fully unrolling the lens functions when possible.
-- The implementation should be semantically equivalent to
--
-- rLens' :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
--        => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
-- rLens' _ f = go implicitly
--   where go :: Elem r rr -> Rec rr f -> g (Rec rr f)
--         go Here (x :& xs) = fmap (:& xs) (f x)
--         go (There p) (x :& xs) = fmap (x :&) (go p xs)

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLens' :: IElem (sy:::t) rs => (sy:::t) -> Lens' (Rec rs f) (f t)
rLens' :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
rLens' _ = (rLens'_unroll :: () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)) () implicitly
{-# INLINE rLens' #-}

rLens'E :: forall r rs sy t f g. (r ~ (sy:::t), Functor g)
       => Elem r rs -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
rLens'E = rLens'_unroll ()
{-# INLINE rLens'E #-}

rLens'_unroll :: (r ~ (sy ::: t), Functor g) => () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
rLens'_unroll _ = rLens'_cont rLens'_unroll
{-# NOINLINE rLens'_unroll #-} -- Inlining would cause the RULE not to fire

rLens'_cont :: (r ~ (sy ::: t), Functor g) 
    => (forall rr'. () -> Elem r rr' -> (f t -> g (f t)) -> Rec rr' f -> g (Rec rr' f))
    -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
rLens'_cont _    Here        f (x :& xs) = fmap (:& xs) (f x)
rLens'_cont cont (There u p) f (x :& xs) = fmap (x :&) (cont u p f xs)
rLens'_cont _    Here        _ _         = error "GHC bug #3927"
rLens'_cont _    (There _ _) _ _         = error "GHC bug #3927"
{-# INLINE rLens'_cont #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not
-- supported. In the parlance of the @lens@ package,
-- 
-- > rLens' :: IElem (sy:::t) rs => (sy:::t) -> Lens' (PlainRec rs) t
rLens :: forall r rs sy t g. (r ~ (sy:::t), IElem r rs, Functor g)
      => r -> (t -> g t) -> PlainRec rs -> g (PlainRec rs)
rLens r = rLens' r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE rLens #-}



-- | subset :: (Functor f, Functor g, ISubset xs ys) => Lens' (Rec xs f) (Rec ys f)
subset :: forall xs ys f g. (Functor f, Functor g, ISubset xs ys)
       => (Rec xs f -> g (Rec xs f)) -> Rec ys f -> g (Rec ys f)
subset = go implicitly
  where
    go s inj ys = (\xs -> setSubset s xs ys) <$> inj (upcast s ys)
    {-# INLINE go #-}
{-# INLINE subset #-}

setSubset :: Functor f => Subset xs ys -> Rec xs f -> Rec ys f -> Rec ys f
setSubset = setSubset_unroll ()
{-# INLINE setSubset #-}

setSubset_unroll :: Functor f => () -> Subset xs ys -> Rec xs f -> Rec ys f -> Rec ys f
setSubset_unroll _ = setSubset_cont setSubset_unroll
{-# NOINLINE setSubset_unroll #-}

setSubset_cont :: Functor f => (forall xs'. () -> Subset xs' ys -> Rec xs' f -> Rec ys f -> Rec ys f)
    -> Subset xs ys -> Rec xs f -> Rec ys f -> Rec ys f
setSubset_cont _    SubsetNil                RNil      = id
setSubset_cont _    SubsetNil                _         = error "GHC bug #3927"
setSubset_cont cont (SubsetCons u x_elem ss) (x :& xs) = rPut'E x_elem x . cont u ss xs
setSubset_cont _    (SubsetCons _ _ _)       _         = error "GHC bug #3927"
{-# INLINE setSubset_cont #-}

rPut'E :: Elem (sy ::: t) rs -> f t -> Rec rs f -> Rec rs f
rPut'E e x = runIdentity . rLens'E e (Identity . const x)
{-# INLINE rPut'E #-}


upcast :: Subset xs ys -> Rec ys f -> Rec xs f
upcast = upcast_unroll ()
{-# INLINE upcast #-}

upcast_unroll :: () -> Subset xs ys -> Rec ys f -> Rec xs f
upcast_unroll _ = upcast_cont upcast_unroll
{-# NOINLINE upcast_unroll #-}

upcast_cont :: (forall xs'. () -> Subset xs' ys -> Rec ys f -> Rec xs' f)
                              -> Subset xs ys  -> Rec ys f -> Rec xs f
upcast_cont _    SubsetNil = \_ -> RNil
upcast_cont cont (SubsetCons u r_elem ss) = \r -> rGet'E r_elem r :& cont u ss r
{-# INLINE upcast_cont #-}

rGet'E :: Elem (sy ::: t) rs -> Rec rs f -> f t
rGet'E e = getConst . rLens'E e Const
{-# INLINE rGet'E #-}

{-# RULES 
"rLens'_unroll" rLens'_unroll () = rLens'_cont rLens'_unroll
"upcast_unroll" upcast_unroll () = upcast_cont upcast_unroll
"setSubset_unroll"   setSubset_unroll () = setSubset_cont setSubset_unroll
    #-}
