{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Vinyl.Prism where
import Control.Applicative ((<$>), Applicative(..))
import Data.Proxy
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel

-- | A prism for record fields. This offers a way to deal with fields
-- that may not be a member of a given record.
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

-- | A lens for the intersection of two records. Provides a way to
-- work with corresponding fields of two records.
class (is ~ RImageMaybe rs ss) => RIntersection rs ss is where
  rintersection :: (Functor g, Applicative f)
                => (Rec f (Intersection2 rs ss) -> g (Rec f (Intersection rs ss)))
                -> (Rec f rs, Rec f ss)
                -> g (Rec f rs, Rec f ss)

instance RIntersection '[] ss '[] where
  rintersection f (RNil, y) = fmap (const (RNil, y)) (f RNil)

instance (RIndexMaybe r ss ~ Nothing, RIntersection rs ss is) =>
  RIntersection (r ': rs) ss (Nothing ': is) where
  rintersection f (x :& xs, y) = fmap aux (rintersection f (xs, y))
    where aux (xs', y') = (x :& xs', y')

instance (RIndexMaybe r ss ~ Just i, RIntersection rs ss is, RElem r ss i, 
          RSubset (Intersection rs ss) rs (RImage (Intersection rs ss) rs),
          RSubset (Intersection rs ss) ss (RImage (Intersection rs ss) ss)) =>
  RIntersection (r ': rs) ss (Just i ': is) where
  rintersection f (x :& xs, y) =
      fmap (\(r' :& rs) ->(r' :& rreplace rs xs, rput r' (rreplace rs y)))
           (f (((,) <$> x <*> rget Proxy y) :& is))
    where Const is = rintersection Const (xs, y)
