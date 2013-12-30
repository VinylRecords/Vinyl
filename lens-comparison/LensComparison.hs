{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
module Main where

import Control.Monad

import LensCombinators
import Functions
import ClassyLens

import Data.Functor.Identity
import System.Random.MWC (Variate(..), create)
import Criterion.Main

import Data.Vinyl.Rec
import Data.Vinyl.Field

type F0 = "0" ::: Int
type F1 = "1" ::: Int
type F2 = "2" ::: Int
type F3 = "3" ::: Int
type F4 = "4" ::: Int
type F5 = "5" ::: Int
type F6 = "6" ::: Int

type Input5 = PlainRec '[F0,F1,F2,F3,F4,F5,X,Y]
type Input6 = PlainRec '[F0,F1,F2,F3,F4,F5,F6,Y,X]
type Input0 = PlainRec '[X,Y]

main :: IO ()
main = do
    gen <- create
    [x1, y1, x2, y2] <- replicateM 4 $ uniform gen
    let input0 :: Input0
        input0 = (Identity x1 :& Identity y1 :& RNil)
        input5 :: Input5
        input5 = (Identity 0 :& Identity 1:& Identity 2 :& Identity 3 :& Identity 4 :& Identity 5 :& Identity x1 :& Identity y1 :& RNil)
        input6 :: Input6
        input6 = (Identity 0 :& Identity 1:& Identity 2 :& Identity 3 :& Identity 4 :& Identity 5 :& Identity 6 :& Identity y2 :& Identity x2 :& RNil)
    defaultMain [ bgroup "baseline"
                  [ bench "sum" $ whnf baselineSum (x1, y1)
                  , bench "distance" $ whnf baselineDistance (x1, y1, x2, y2)
                  ]
                , bgroup "simple"
                  [ bench "sum5" $ whnf simpleSum input5
                  , bench "distance-0-0" $ whnf simpleDistance (input0, input0)
                  , bench "distance-5-5" $ whnf simpleDistance (input5, input5)
                  , bench "distance-5-6" $ whnf simpleDistance (input5, input6)
                  ]
                , bgroup "rLens"
                  [ bench "sum5" $ whnf rLens'Sum input5
                  , bench "distance-0-0" $ whnf rLens'Distance (input0, input0)
                  , bench "distance-5-5" $ whnf rLens'Distance (input5, input5)
                  , bench "distance-5-6" $ whnf rLens'Distance (input5, input6)
                  ]
                , bgroup "classy"
                  [ bench "sum5" $ whnf classySum input5
                  , bench "distance-0-0" $ whnf classyDistance (input0, input0)
                  , bench "distance-5-5" $ whnf classyDistance (input5, input5)
                  , bench "distance-5-6" $ whnf classyDistance (input5, input6)
                  ]
                , bgroup "unrolled"
                  [ bench "sum5" $ whnf unrolledSum input5
                  , bench "distance-0-0" $ whnf unrolledDistance (input0, input0)
                  , bench "distance-5-5" $ whnf unrolledDistance (input5, input5)
                  , bench "distance-5-6" $ whnf unrolledDistance (input5, input6)
                  ]
                , bgroup "unrolled_inline"
                  [ bench "sum5" $ whnf unrolledSum_inlinable input5
                  , bench "distance-0-0" $ whnf unrolledDistance_inline (input0, input0)
                  , bench "distance-5-5" $ whnf unrolledDistance_inline (input5, input5)
                  , bench "distance-5-6" $ whnf unrolledDistance_inline (input5, input6)
                  ]
                ]

{-
I whipped up a benchmark of the a few lens implementations--just adding two fields together, i.e. two `get`s, with no `put` or `mod`. I benchmarked the current implementation, a simplified version without the hand-unrolling, the RULES-based unrolling discussed in this thread, and a typeclass-based implementation. 

You can see my results in Windows with GHC 7.6, but *not* LLVM, [here](https://dl.dropboxusercontent.com/u/18852077/20131229%20Vinyl%20lens%20benchmarks.html). Sorry for the noisy data. A few observations:

- Everything but the simplified version works well when accessing the first two fields. Sum functions of type `PlainRec '["X" ::: Double, "Y" ::: Double] -> Double` are only a few nanoseconds away from `uncurry (+)`--roughly 10ns vs. 12ns. 

- Adding five dummy fields before X and Y adds another two nanoseconds to both the unrolled and classy lenses; a total of about 50% overhead over `uncurry (+)`. The current implementation falls behind, adding about 20ns/200% overhead. But it's better than the naive implementation, with ~45ns overhead.

- Both the unrolling and classy lenses are considerably *slower* than the current implementation when GHC can't inline/specialize them. The RULES-based unrolling solution, with five dummy fields, adds about 200ns; about ten times *worse* than the current implementation.
-}