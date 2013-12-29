{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FlexibleInstances         #-}


{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
module ClassyLens (HasLens(..)) where

import Data.Vinyl.Rec
import Data.Vinyl.Field

class HasLens sy t xs where
  classyLens :: Functor g => (sy ::: t) -> (f t -> g (f t)) -> Rec xs f -> g (Rec xs f)
  
instance HasLens sy t ((sy ::: t) ': rs) where
  classyLens _ f (x :& xs) = fmap (:& xs) (f x)

instance HasLens sy t rs
    => HasLens sy t ((y ': rs)) where
  classyLens p f (x :& xs) = fmap (x :&) (classyLens p f xs)
