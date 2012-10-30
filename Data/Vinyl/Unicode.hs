{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Vinyl.Unicode where

import Data.Vinyl.Relation
import Data.Vinyl.Proofs

type x ∈ xs = IElem x xs
type xs ⊆ ys = ISubset xs ys
type r1 ≅ r2 = r1 :~: r2
