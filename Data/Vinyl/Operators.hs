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
  , showWithNames
  , rshow
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TyFun
import Data.Vinyl.Witnesses
import Data.Vinyl.Constraint
import Data.Vinyl.Derived
import qualified Data.Vinyl.Idiom.Identity as I
import qualified Data.Vinyl.Idiom.Thunk as I
import qualified Data.Vinyl.Universe.Const as U

import Control.Applicative
import qualified Data.List as L (intercalate)

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

rdist :: Applicative f => Rec el f rs -> f (PlainRec el rs)
rdist = rtraverse $ fmap I.Identity

rdistLazy :: Applicative f => Rec el f rs -> f (LazyPlainRec el rs)
rdistLazy = rtraverse $ fmap I.Thunk

showWithNames :: RecAll el f rs Show => PlainRec (U.Const String) rs -> Rec el f rs -> String
showWithNames names rec = "{ " ++ L.intercalate ", " (go names rec []) ++ " }"
  where
    go :: RecAll el f rs Show => PlainRec (U.Const String) rs -> Rec el f rs -> [String] -> [String]
    go RNil RNil ss = ss
    go (I.Identity n :& ns) (x :& xs) ss = (n ++ " =: " ++ show x) : go ns xs ss

rshow :: (Implicit (PlainRec (U.Const String) rs), RecAll el f rs Show) => Rec el f rs -> String
rshow = showWithNames implicitly

