{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vinyl.Derived where

import Data.Vinyl.Core
import Data.Vinyl.Functor

import Control.Applicative
import GHC.TypeLits

data ElField (field :: (Symbol, *)) where
  Field :: KnownSymbol s => t -> ElField '(s,t)

type FieldRec = Rec ElField
type HList = Rec Identity
type LazyHList = Rec Thunk
