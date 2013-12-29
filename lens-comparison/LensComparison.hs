{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
module Main where

import LensCombinators
import Functions

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

type Input5 = PlainRec '[F0,F1,F2,F3,F4,F5,X,Y]
type Input0 = PlainRec '[X,Y]

main :: IO ()
main = do
    gen <- create
    x <- uniform gen
    y <- uniform gen
    let input5 :: Input5
        input5 = (Identity 0 :& Identity 1:& Identity 2 :& Identity 3 :& Identity 4 :& Identity 5 :& Identity x :& Identity y :& RNil)
        input0 :: Input0
        input0 = (Identity x :& Identity y :& RNil)
    defaultMain [ bench "baseline" $ whnf (uncurry (+)) (x, y)
                , bench "simpleSum0" $ whnf simpleSum input0
                , bench "unrolledSum0" $ whnf unrolledSum input0
                , bench "classySum0" $ whnf classySum input0
                , bench "unrolledSum_noinline0" $ whnf unrolledSum_noinline input0
                , bench "classySum_noinline0" $ whnf classySum_noinline input0
                , bench "simpleSum5" $ whnf simpleSum input5
                , bench "unrolledSum5" $ whnf unrolledSum input5
                , bench "classySum5" $ whnf classySum input5
                , bench "unrolledSum_noinline5" $ whnf unrolledSum_noinline input5
                , bench "classySum_noinline5" $ whnf classySum_noinline input5
                ]
