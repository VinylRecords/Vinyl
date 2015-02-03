{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vinyl.TypeLevel where

import GHC.Exts

-- | A mere approximation of the natural numbers. And their image as lifted by
-- @-XDataKinds@ corresponds to the actual natural numbers.
data Nat = Z | S !Nat

-- | A partial relation that gives the index of a value in a list.
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = Z
  RIndex r (s ': rs) = S (RIndex r rs)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

-- | A constraint-former which applies to every field in a record.
type family RecAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  RecAll f '[] c = ()
  RecAll f (r ': rs) c = (c (f r), RecAll f rs c)

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type family FmapMaybe f t where
  FmapMaybe f Nothing = Nothing
  FmapMaybe f (Just t) = Just (f t)

type family RIndexMaybe (r :: k) (rs :: [k]) :: Maybe Nat where
  RIndexMaybe r '[] = 'Nothing
  RIndexMaybe r (r ': rs) = Just Z
  RIndexMaybe r (s ': rs) = FmapMaybe S (RIndexMaybe r rs)

type family FmapMaybeConst a t where
  FmapMaybeConst a Nothing = Nothing
  FmapMaybeConst a (Just t) = Just a

type family Intersection' rs ss where
  Intersection' '[] ss = '[]
  Intersection' (r ': rs) ss = FmapMaybeConst r (RIndexMaybe r ss)
                               ': Intersection' rs ss

type Intersection rs ss = CatMaybes (Intersection' rs ss)

type family Intersection2' rs ss where
  Intersection2' '[] ss = '[]
  Intersection2' (r ': rs) ss = FmapMaybeConst (r,r) (RIndexMaybe r ss)
                               ': Intersection2' rs ss

type Intersection2 rs ss = CatMaybes (Intersection2' rs ss)

type family CatMaybes rs where
  CatMaybes '[] = '[]
  CatMaybes (Nothing ': rs) = CatMaybes rs
  CatMaybes (Just r ': rs) = r ': CatMaybes rs

type family RImageMaybe rs ss :: [Maybe Nat] where
  RImageMaybe '[] ss = '[]
  RImageMaybe (r ': rs) ss = RIndexMaybe r ss ': RImageMaybe rs ss
