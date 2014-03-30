{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Operators
  ( (<<$>>)
  , (<<*>>)
  , (<+>)
  , rpure
  , rtraverse
  , rdist
  , rdistLazy
  , foldRec
  , recToList
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Idiom.LazyIdentity
import Control.Applicative

-- | Append for records.
(<+>) :: Rec as f -> Rec bs f -> Rec (as ++ bs) f
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixr 5  <+>

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)

(<<$>>) :: (forall x. (f ~> g) x) -> Rec rs f -> Rec rs g
_   <<$>> RNil    = RNil
eta <<$>> x :& xs = runNT eta x :& (eta <<$>> xs)
infixl 8 <<$>>
{-# INLINE (<<$>>) #-}

(<<*>>) :: Rec rs (f ~> g) -> Rec rs f -> Rec rs g
RNil    <<*>> RNil    = RNil
f :& fs <<*>> x :& xs = runNT f x :& (fs <<*>> xs)
infixl 8 <<*>>
{-# INLINE (<<*>>) #-}

class RecApplicative rs where
  rpure :: (forall x. f x) -> Rec rs f
instance RecApplicative '[] where
  rpure _ = RNil
instance RecApplicative rs => RecApplicative (f ': rs) where
  rpure s = s :& rpure s

class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b
instance FoldRec (Rec '[] f) a where
  foldRec _ z RNil = z
instance (t ~ El r, FoldRec (Rec rs f) (f t)) => FoldRec (Rec (r ': rs) f) (f t) where
  foldRec f z (x :& xs) = f x (foldRec f z xs)

-- | Accumulates a homogenous record into a list
recToList :: FoldRec (Rec rs f) (f t) => Rec rs f -> [f t]
recToList = foldRec (\e a -> [e] ++ a) []

rtraverse :: Applicative h => (forall x. f x -> h (g x)) -> Rec rs f -> h (Rec rs g)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs

rdist :: (Applicative f) => Rec rs f -> f (Rec rs Identity)
rdist = rtraverse $ fmap Identity

rdistLazy :: (Applicative f) => Rec rs f -> f (Rec rs LazyIdentity)
rdistLazy = rtraverse $ fmap LazyIdentity
