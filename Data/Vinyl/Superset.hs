{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vinyl.Superset where

import Data.Vinyl.Core
  ( DictOnly (DictOnly),
    RPureConstrained,
    Rec,
    rpureConstrained,
  )
import Data.Vinyl.Lens (rcast, rget)
import Data.Vinyl.Notation (type (∈), type (⊆))

-- Flips the order of the operands to the usual `∈` Constraint former
-- for use with types like `V.DictOnly`. For example, @Rec (DictOnly
-- (HasElem ss)) rs@ is a record of evidence that each element of @rs@
-- is an element of @ss@.
class a ∈ rs => HasElem rs a

instance a ∈ rs => HasElem rs a

-- | Generate evidence that every element of a list of types is an
-- element of that list of types. This gives us a first-class way of
-- handling evidence @r ∈ rs@ for each element of a list @rs@. This is
-- particular useful when helping the type checker see through subset
-- relations.
allElems :: forall rs. (RPureConstrained (HasElem rs) rs)
         => Rec (DictOnly (HasElem rs)) rs
allElems = rpureConstrained @(HasElem rs) DictOnly

-- | @allElemsOfSubset \@rs \@ss@ produces evidence that if @rs@ is a
-- subset of @ss@, then every element of @rs@ is an element of @ss@.
allElemsOfSuperset :: forall rs ss.
                      (rs ⊆ ss, RPureConstrained (HasElem ss) ss)
                   => Rec (DictOnly (HasElem ss)) rs
allElemsOfSuperset = rcast @rs (allElems @ss)

-- | If we have evidence that @r ∈ rs@ and that @rs ⊆ ss@, then we can
-- apply a function that requires that @r ∈ ss@.
onSuperset :: forall r rs ss f x.
              (r ∈ rs, rs ⊆ ss, RPureConstrained (HasElem ss) ss)
           => (r ∈ ss => Rec f ss -> x) -> Rec f ss -> x
onSuperset = let proof = allElemsOfSuperset @rs @ss
             in \f x -> case rget @r proof of
                          DictOnly -> f x
{-# INLINE onSuperset #-}
