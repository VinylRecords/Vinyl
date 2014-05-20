{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Derived where

import Data.Vinyl.Core
import Data.Vinyl.TyFun
import qualified Data.Vinyl.Idiom.Identity as I
import qualified Data.Vinyl.Idiom.Thunk as I
import qualified Data.Vinyl.Universe as U

import Control.Applicative

type LazyRec el = Rec (I.Thunk :. el)
type FieldRec = Rec U.ElField
type PlainFieldRec = Rec U.ElField
type HList = Rec U.Id
type LazyHList = Rec (TC I.Thunk)

