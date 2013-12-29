{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Functions where

import LensCombinators
import UnrolledLens
import SimpleLens
import ClassyLens

import Data.Vinyl.Rec
import Data.Vinyl.Field

type X = "X" ::: Double
type Y = "Y" ::: Double
type Res = "Result" ::: Double

x :: X
x = Field
y :: Y
y = Field
res :: Res
res = Field


simpleSum = \r -> go r
  where go r = x' + y'
          where x' = getI (simpleLens x) r
                y' = getI (simpleLens y) r
{-# INLINE simpleSum #-}

simpleSum_noinline r = x' + y'
  where x' = getI (simpleLens x) r
        y' = getI (simpleLens y) r
{-# INLINE simpleSum_noinline #-}

unrolledSum = \r -> go r
  where go r = x' + y'
          where x' = getI (unrolledLens x) r
                y' = getI (unrolledLens y) r
{-# INLINE unrolledSum #-}

unrolledSum_noinline r = x' + y'
  where x' = getI (unrolledLens x) r
        y' = getI (unrolledLens y) r
{-# INLINE unrolledSum_noinline #-}

classySum = \r -> go r
  where go r = x' + y'
          where x' = getI (classyLens x) r
                y' = getI (classyLens y) r
{-# INLINE classySum #-}

classySum_noinline r = x' + y'
  where x' = getI (classyLens x) r
        y' = getI (classyLens y) r
{-# INLINE classySum_noinline #-}
