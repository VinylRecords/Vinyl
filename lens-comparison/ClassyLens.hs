{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, ConstraintKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances   #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

module ClassyLens (HasField(..), HasFields) where

import Data.Vinyl.Rec
import Data.Vinyl.Field
import GHC.Exts (Constraint)

class HasField x rs where
  classyLens :: (Functor g, x ~ (sy ::: t)) => (sy ::: t) -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
  
instance HasField (sy ::: t) ((sy ::: t) ': rs) where
  classyLens _ f (x :& xs) = fmap (:& xs) (f x)

instance HasField (sy ::: t) rs
    => HasField (sy ::: t) ((y ': rs)) where
  classyLens p f (x :& xs) = fmap (x :&) (classyLens p f xs)

-- GHC bug #7785 (fixed) prevents specialization of functions using this constraint.
-- Wrapping it in a class fixes this.
type family HasFields' (xs :: [*]) (rs :: [*]) :: Constraint
type instance HasFields' '[] rs = ()
type instance HasFields' (x ': xs) rs = (HasField x rs, HasFields xs rs)

class HasFields' xs rs => HasFields xs rs
instance HasFields' xs rs => HasFields xs rs