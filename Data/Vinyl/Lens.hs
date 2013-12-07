{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
-- | A small, /en passant/ lens implementation to provide accessors
-- for record fields. Lenses produced with 'field are fully
-- compatible with the @lens@ package.
module Data.Vinyl.Lens 
    ( field, field'
    , getField, getField'
    , putField, putField'
    , modField
    , subset
    ) where
    -- ( getField
    -- , rGet
    -- , putField
    -- , putField'
    -- , rMod
    -- , field
    -- , rLens
    -- , upcast
    -- ) 
import Control.Applicative
import Data.Functor.Identity
import Data.Vinyl.Field
import Data.Vinyl.Rec
import Data.Vinyl.Witnesses

-- | Project a field from a 'Rec'.
getField :: IElem (sy ::: t) rs => (sy ::: t) -> Rec rs f -> f t
getField r = getConst . field r Const
{-# INLINE getField #-}

-- | Project a field from a 'PlainRec'.
getField' :: IElem (sy ::: t) rs => (sy ::: t) -> PlainRec rs -> t
getField' = (runIdentity .) . getField
{-# INLINE getField' #-}

-- | Set a field in a 'Rec' over an arbitrary functor.
putField :: IElem (sy ::: t) rs => (sy ::: t) -> f t -> Rec rs f -> Rec rs f
putField r x = runIdentity . field r (Identity . const x)
{-# INLINE putField #-}

-- | Set a field in a 'PlainRec'.
putField' :: IElem (sy:::t) rs => (sy:::t) -> t -> PlainRec rs -> PlainRec rs
putField' r x = putField r (Identity x)
{-# INLINE putField' #-}

-- | Modify a field.
modField :: (IElem (sy:::t) rs, Functor f)
     => (sy:::t) -> (t -> t) -> Rec rs f -> Rec rs f
modField r f = runIdentity . field r (Identity . fmap f)
{-# INLINE modField #-}

-- This unusual implementaiton in based on 
-- http://unlines.wordpress.com/2009/11/05/tricking-ghc-into-evaluating-recursive-functions-at-compile-time/
-- and should result in GHC fully unrolling the lens functions when possible

-- | Provide a lens to a record field. Note that this implementation
-- does not support polymorphic update. In the parlance of the @lens@
-- package,
--
-- > field :: IElem (sy:::t) rs => (sy:::t) -> Lens' (Rec rs f) (f t)
field :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
field _ = (field_unroll :: () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)) () implicitly
{-# INLINE field #-}

fieldE :: forall r rs sy t f g. (r ~ (sy:::t), Functor g)
       => Elem r rs -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
fieldE = field_unroll ()
{-# INLINE fieldE #-}

field_unroll :: (r ~ (sy ::: t), Functor g) => () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
field_unroll _ = field_cont field_unroll
{-# NOINLINE field_unroll #-} -- Inlining would cause the RULE not to fire

field_cont :: (r ~ (sy ::: t), Functor g) 
    => (forall rr'. () -> Elem r rr' -> (f t -> g (f t)) -> Rec rr' f -> g (Rec rr' f))
    -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
field_cont _    Here        f (x :& xs) = fmap (:& xs) (f x)
field_cont cont (There u p) f (x :& xs) = fmap (x :&) (cont u p f xs)
field_cont _    Here        _ _         = error "GHC bug #3927"
field_cont _    (There _ _) _ _         = error "GHC bug #3927"
{-# INLINE field_cont #-}

-- | A lens into a 'PlainRec' that smoothly interoperates with lenses
-- from the @lens@ package. Note that polymorphic update is not
-- supported. In the parlance of the @lens@ package,
-- 
-- > rLens :: IElem (sy:::t) rs => (sy:::t) -> Lens' (PlainRec rs) t
field' :: forall r rs sy t g. (r ~ (sy:::t), IElem r rs, Functor g)
      => r -> (t -> g t) -> PlainRec rs -> g (PlainRec rs)
field' r = field r . lenser runIdentity (const Identity)
  where lenser sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE field' #-}

-- 
-- The implementation of field above should be equivalent to:
-- field :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
--        => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
-- field _ f = go implicitly
--   where go :: Elem r rr -> Rec rr f -> g (Rec rr f)
--         go Here (x :& xs) = fmap (:& xs) (f x)
--         go (There p) (x :& xs) = fmap (x :&) (go p xs)

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
setSubset_cont cont (SubsetCons u x_elem ss) (x :& xs) = putFieldE x_elem x . cont u ss xs
setSubset_cont _    (SubsetCons _ _ _)       _         = error "GHC bug #3927"
{-# INLINE setSubset_cont #-}

putFieldE :: Elem (sy ::: t) rs -> f t -> Rec rs f -> Rec rs f
putFieldE e x = runIdentity . fieldE e (Identity . const x)
{-# INLINE putFieldE #-}


upcast :: Subset xs ys -> Rec ys f -> Rec xs f
upcast = upcast_unroll ()
{-# INLINE upcast #-}

upcast_unroll :: () -> Subset xs ys -> Rec ys f -> Rec xs f
upcast_unroll _ = upcast_cont upcast_unroll
{-# NOINLINE upcast_unroll #-}

upcast_cont :: (forall xs'. () -> Subset xs' ys -> Rec ys f -> Rec xs' f)
                              -> Subset xs ys  -> Rec ys f -> Rec xs f
upcast_cont _    SubsetNil = \_ -> RNil
upcast_cont cont (SubsetCons u r_elem ss) = \r -> getFieldE r_elem r :& cont u ss r
{-# INLINE upcast_cont #-}

getFieldE :: Elem (sy ::: t) rs -> Rec rs f -> f t
getFieldE e = getConst . fieldE e Const
{-# INLINE getFieldE #-}

{-# RULES 
"field_unroll" field_unroll () = field_cont field_unroll
"upcast_unroll" upcast_unroll () = upcast_cont upcast_unroll
"setSubset_unroll"   setSubset_unroll () = setSubset_cont setSubset_unroll
    #-}
