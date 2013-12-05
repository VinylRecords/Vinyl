{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields. Lenses produced with 'rLens' are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens 
    ( rGet'
    , rGet
    , rPut'
    , rPut
    , rMod
    , rLens'
    , rLens
    , cast'
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

rGet'FromElem :: Elem (sy ::: t) rs -> Rec rs f -> f t
rGet'FromElem e = getConst . rLens'FromElem e Const
{-# INLINE rGet'FromElem #-}

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

-- This unusual implementaiton in based on 
-- http://unlines.wordpress.com/2009/11/05/tricking-ghc-into-evaluating-recursive-functions-at-compile-time/
-- and should result in GHC fully unrolling the lens functions when possible

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > rLens' :: IElem (sy:::t) rs => (sy:::t) -> Lens' (Rec rs f) (f t)
rLens' :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
rLens' _ = (rLens'_unroll :: () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)) () implicitly
{-# INLINE rLens' #-}

rLens'FromElem :: forall r rs sy t f g. (r ~ (sy:::t), Functor g)
       => Elem r rs -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
rLens'FromElem = (rLens'_unroll :: () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)) ()
{-# INLINE rLens'FromElem #-}

rLens'_unroll :: (r ~ (sy ::: t), Functor g) => () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
rLens'_unroll _ = rLens'_cont rLens'_unroll
{-# NOINLINE rLens'_unroll #-} -- Inlining would cause the RULE not to fire

rLens'_cont :: (r ~ (sy ::: t), Functor g) 
    => (forall rr'. () -> Elem r rr' -> (f t -> g (f t)) -> Rec rr' f -> g (Rec rr' f))
    -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
rLens'_cont cont Here f (x :& xs) = fmap (:& xs) (f x)
rLens'_cont cont (There u p) f  (x :& xs) = fmap (x :&) (cont u p f xs)
{-# INLINE rLens'_cont #-}

{-# RULES 
    "rLens'_unroll'" rLens'_unroll () = rLens'_cont rLens'_unroll
    #-}

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

cast' :: Subset ys' xs -> Rec xs f -> Rec ys' f
cast' = (cast'_unroll :: () -> Subset ys' xs -> Rec xs f -> Rec ys' f) ()
{-# INLINE cast' #-}

cast'_unroll :: () -> Subset ys' xs -> Rec xs f -> Rec ys' f
cast'_unroll _ = cast'_cont cast'_unroll
{-# NOINLINE cast'_unroll #-}

cast'_cont :: (forall ys'. () -> Subset ys' xs -> Rec xs f -> Rec ys' f)
        -> Subset ys xs -> Rec xs f -> Rec ys f
cast'_cont cont SubsetNil = \_ -> RNil
cast'_cont cont (SubsetCons u elem ss) = \r -> rGet'FromElem elem r :& cont u ss r
{-# INLINE cast'_cont #-}

{-# RULES "cast'_unroll" cast'_unroll () = cast'_cont cast'_unroll #-}
