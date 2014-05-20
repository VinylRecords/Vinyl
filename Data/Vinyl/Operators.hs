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
  , foldRec
  , recToList
  , showWithNames
  , rshow
  , leftComposeIdentity
  , rightComposeIdentity
  , leftComposeIdentity'
  , rightComposeIdentity'
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TyFun
import Data.Vinyl.Witnesses
import Data.Vinyl.Constraint
import qualified Data.Vinyl.Universe.Id as U
import qualified Data.Vinyl.Universe.Const as U

import Control.Applicative
import qualified Data.List as L (intercalate)

import qualified Unsafe.Coerce as Funext

-- | Append for records.
(<+>) :: Rec el as -> Rec el bs -> Rec el (as ++ bs)
RNil      <+> xs = xs
(x :& xs) <+> ys =  x :& (xs <+> ys)
infixr 5  <+>

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs  = a ': (as ++ bs)

(<<$>>) :: (forall x. f x -> g x) -> Rec (f :. el) rs -> Rec (g :. el) rs
_   <<$>> RNil    = RNil
eta <<$>> x :& xs = eta x :& (eta <<$>> xs)
infixl 8 <<$>>
{-# INLINE (<<$>>) #-}

(<<*>>) :: Rec (Lift (->) f g :. el) rs -> Rec (Compose f  el) rs -> Rec (Compose g el) rs
RNil    <<*>> RNil    = RNil
f :& fs <<*>> x :& xs = runLift f x :& (fs <<*>> xs)
infixl 8 <<*>>
{-# INLINE (<<*>>) #-}

class RecApplicative rs where
  rpure :: (forall x. f x) -> Rec (f :. el) rs
instance RecApplicative '[] where
  rpure _ = RNil
instance RecApplicative rs => RecApplicative (f ': rs) where
  rpure s = s :& rpure s

class FoldRec r a where
  foldRec :: (a -> b -> b) -> b -> r -> b
instance FoldRec (Rec el '[]) a where
  foldRec _ z RNil = z
instance (t ~ (el $ r), FoldRec (Rec el rs) t) => FoldRec (Rec el (r ': rs)) t where
  foldRec f z (x :& xs) = f x (foldRec f z xs)

-- | Accumulates a homogenous record into a list
recToList :: FoldRec (Rec el rs) t => Rec el rs -> [t]
recToList = foldRec (\e a -> [e] ++ a) []

rtraverse :: Applicative h => (forall x. f x -> h (g $ x)) -> Rec (f :. el) rs -> h (Rec (Compose g el) rs)
rtraverse _ RNil = pure RNil
rtraverse f (x :& xs) = (:&) <$> f x <*> rtraverse f xs

rdist :: Applicative f => Rec (f :. el) rs -> f (Rec el rs)
rdist = Funext.unsafeCoerce . rdist'
  where
    rdist' :: Applicative f => Rec (f :. el) rs -> f (Rec (Compose U.Id el) rs)
    rdist' = rtraverse id

showWithNames :: RecAll el rs Show => Rec (U.Const String) rs -> Rec el rs -> String
showWithNames names rec = "{ " ++ L.intercalate ", " (go names rec []) ++ " }"
  where
    go :: RecAll el rs Show => Rec (U.Const String) rs -> Rec el rs -> [String] -> [String]
    go RNil RNil ss = ss
    go (n :& ns) (x :& xs) ss = (n ++ " =: " ++ show x) : go ns xs ss

rshow :: (Implicit (Rec (U.Const String) rs), RecAll el rs Show) => Rec el rs -> String
rshow = showWithNames implicitly

leftComposeIdentity :: Rec (Compose U.Id el) rs -> Rec el rs
leftComposeIdentity = Funext.unsafeCoerce

leftComposeIdentity' :: Rec el rs -> Rec (Compose U.Id el) rs
leftComposeIdentity' = Funext.unsafeCoerce

rightComposeIdentity :: Rec (Compose el U.Id) rs -> Rec el rs
rightComposeIdentity = Funext.unsafeCoerce

rightComposeIdentity' :: Rec el rs -> Rec (Compose el U.Id) rs
rightComposeIdentity' = Funext.unsafeCoerce

