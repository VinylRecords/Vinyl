{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts                 #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Functions where

import LensCombinators
import UnrolledLens
import SimpleLens
import ClassyLens

import Data.Vinyl.Rec
import Data.Vinyl.Field

import Criterion.Main

type X = "X" ::: Double
type Y = "Y" ::: Double

x :: X
x = Field
y :: Y
y = Field

-- Even the baseline distance function must be marked INLINABLE
-- so that GHC can specialize away the Floating class dictionary.
-- Otherwise performance is much worse.
baselineDistance (x1, y1, x2, y2) = sqrt $ dx * dx + dy * dy
  where dx = (x1 + 1) - (x2 + 1)
        dy = (y1 + 1) - (y2 + 1)
{-# INLINABLE baselineDistance #-}

simpleDistance (r1, r2) = sqrt $ dx * dx + dy * dy
  where alter = modI (l x) (+1) . modI (l y) (+1)
        r1' = alter r1
        r2' = alter r2
        dx = get x r1' - get x r2'
        dy = get y r1' - get y r2'
        get f = getI (l f)
        l = simpleLens
{-# INLINABLE simpleDistance #-}

rLens'Distance (r1, r2) = sqrt $ dx * dx + dy * dy
  where alter = modI (l x) (+1) . modI (l y) (+1)
        r1' = alter r1
        r2' = alter r2
        dx = get x r1' - get x r2'
        dy = get y r1' - get y r2'
        get f = getI (l f)
        l = rLens'
{-# INLINABLE rLens'Distance #-}
        
classyDistance :: ( HasFields '[X, Y] rs1
                  , HasFields '[X, Y] rs2)
                  => (PlainRec rs1, PlainRec rs2) -> Double
classyDistance (r1, r2) = sqrt $ dx * dx + dy * dy
  where alter = modI (l x) (+1) . modI (l y) (+1)
        r1' = alter r1
        r2' = alter r2
        dx = get x r1' - get x r2'
        dy = get y r1' - get y r2'
        get f = getI (l f)
        l = classyLens
{-# INLINABLE classyDistance #-}
        
unrolledDistance (r1, r2) = sqrt $ dx * dx + dy * dy
  where alter = modI (l x) (+1) . modI (l y) (+1)
        r1' = alter r1
        r2' = alter r2
        dx = get x r1' - get x r2'
        dy = get y r1' - get y r2'
        get f = getI (l f)
        l = unrolledLens
{-# INLINABLE unrolledDistance #-}

unrolledDistance_inline = 
  \(r1, r2) -> let  alter = modI (l x) (+1) . modI (l y) (+1)
                    r1' = alter r1
                    r2' = alter r2
                    dx = get x r1' - get x r2'
                    dy = get y r1' - get y r2'
                    get f = getI (l f)
                    l = unrolledLens
                    {-# INLINE alter #-}
                    {-# INLINE get #-}
               in sqrt $ dx * dx + dy * dy
{-# INLINE unrolledDistance_inline #-}

-- Marking these simple sum functions INLINABLE does not have much effect,
-- except in the case of unrolledSum, where omitting it catastrophically reduces
-- performance.

baselineSum (x, y) = x + y

simpleSum r = x' + y'
  where x' = getI (simpleLens x) r
        y' = getI (simpleLens y) r

rLens'Sum r = x' + y'
  where x' = getI (rLens' x) r
        y' = getI (rLens' y) r

classySum r = x' + y'
  where x' = getI (classyLens x) r
        y' = getI (classyLens y) r

unrolledSum r = x' + y'
  where x' = getI (unrolledLens x) r
        y' = getI (unrolledLens y) r

unrolledSum_inlinable r = x' + y'
  where x' = getI (unrolledLens x) r
        y' = getI (unrolledLens y) r
{-# INLINABLE unrolledSum_inlinable #-}

-- The above INLINABLE version performs much the same as the INLINE version
-- below.

{-
unrolledSum_inline = \r -> go r
  where go r = x' + y'
          where x' = getI (unrolledLens x) r
                y' = getI (unrolledLens y) r
{-# INLINE unrolledSum_inline #-}
-}
