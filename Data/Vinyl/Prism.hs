{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Vinyl.Prism where
import Control.Applicative
import Data.Vinyl.Core
import Data.Vinyl.TypeLevel

class i ~ RIndexMaybe r rs => RPrism (r :: k) (rs :: [k]) (i :: Maybe Nat) where
  rprism :: Applicative g
         => sing r
         -> (f r -> g (f r))
         -> Rec f rs
         -> g (Rec f rs)

instance (RIndexMaybe r rs ~ Nothing) => RPrism r rs Nothing where
  rprism _ _ r = pure r
  {-# INLINE rprism #-}

instance RPrism r (r ': rs) (Just Z) where
  rprism _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE rprism #-}

instance (RIndexMaybe r (s ': rs) ~ Just (S i), RPrism r rs (Just i)) =>
    RPrism r (s ': rs) (Just (S i)) where
  rprism p f (x :& xs) = fmap (x :&) (rprism p f xs)
  {-# INLINE rprism #-}
