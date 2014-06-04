{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vinyl.Derived where

import Data.Vinyl.Core
import qualified Data.Vinyl.Idiom.Identity as I
import qualified Data.Vinyl.Idiom.Thunk as I
import qualified Data.Vinyl.Universe as U

import Control.Applicative

type PlainRec el = Rec el I.Identity
type LazyPlainRec el = Rec el I.Thunk
type FieldRec = Rec U.ElField
type PlainFieldRec = Rec U.ElField I.Identity
type HList = Rec U.Id I.Identity
type LazyHList = Rec U.Id I.Thunk

-- | Fixes a polymorphic record into the 'Identity' functor.
toPlainRec :: (forall f. Applicative f => Rec el f rs) -> PlainRec el rs
toPlainRec xs = xs

toLazyPlainRec :: (forall f. Applicative f => Rec el f rs) -> LazyPlainRec el rs
toLazyPlainRec xs = xs

