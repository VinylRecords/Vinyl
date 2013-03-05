{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Data.Vinyl.Unicode where

import           Data.Vinyl.Rec
import           Data.Vinyl.Relation
import           Data.Vinyl.Witnesses

type x ∈ xs = IElem x xs
type xs ⊆ ys = ISubset xs ys
type r1 ≅ r2 = r1 :~: r2

(≅) = (~=)
