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
import Data.Vinyl.Lift
import Data.Vinyl.TyFun
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Idiom.LazyIdentity
import Control.Applicative

-- | Append for records.
(<+>) :: Rec el f as -> Rec el f bs -> Rec el f (as ++ bs)
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixr 5  <+>

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)

(<<$>>) :: (forall x. f x -> g x) -> Rec el f rs -> Rec el g rs
_   <<$>> RNil    = RNil
eta <<$>> x :& xs = eta x :& (eta <<$>> xs)
infixl 8 <<$>>
{-# INLINE (<<$>>) #-}

(<<*>>) :: Rec el (Lift (->) f g) rs -> Rec el f rs -> Rec el g rs
RNil    <<*>> RNil    = RNil
f :& fs <<*>> x :& xs = runLift f x :& (fs <<*>> xs)
infixl 8 <<*>>
{-# INLINE (<<*>>) #-}

class RecApplicative rs where
  rpure :: (forall x. f x) -> Rec el f rs
instance RecApplicative '[] where
  rpure _ = RNil
instance RecApplicative rs => RecApplicative (f ': rs) where
  rpure s = s :& rpure s

class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b
instance FoldRec (Rec el f '[]) a where
  foldRec _ z RNil = z
instance (t ~ (el $ r), FoldRec (Rec el f rs) (f t)) => FoldRec (Rec el f (r ': rs)) (f t) where
  foldRec f z (x :& xs) = f x (foldRec f z xs)

-- | Accumulates a homogenous record into a list
recToList :: FoldRec (Rec el f rs) (f t) => Rec el f rs -> [f t]
recToList = foldRec (\e a -> [e] ++ a) []

rtraverse :: Applicative h => (forall x. f x -> h (g x)) -> Rec el f rs -> h (Rec el g rs)
rtraverse _ RNil      = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs

rdist :: Applicative f => Rec el f rs -> f (Rec el Identity rs)
rdist = rtraverse $ fmap Identity

rdistLazy :: Applicative f => Rec el f rs -> f (Rec el LazyIdentity rs)
rdistLazy = rtraverse $ fmap LazyIdentity
