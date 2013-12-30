{-# LANGUAGE RankNTypes #-}
module LensCombinators where

import Data.Vinyl.Rec
import Data.Functor.Identity
import Control.Applicative

getI :: (forall f g. (Functor f, Functor g) => (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)) -> PlainRec rs -> t
getI l = (runIdentity .) $ get l
{-# INLINE getI #-}

get :: Functor f => (forall g. Functor g => (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)) -> Rec rs f -> f t
get l = getConst . l Const
{-# INLINE get #-}

putI :: (forall f g. (Functor f, Functor g) => (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)) -> t -> PlainRec rs -> PlainRec rs
putI l x = runIdentity . l (Identity . const (Identity x))
{-# INLINE putI #-}

modI :: (forall f g. (Functor f, Functor g) => (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)) -> (t -> t) -> PlainRec rs -> PlainRec rs
modI r f = runIdentity . r (Identity . fmap f)
{-# INLINE modI #-}