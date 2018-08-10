{-# language DataKinds, TypeOperators, TypeApplications #-}
import Data.Vinyl.CoRec
import Data.Vinyl.Functor (Identity(..))
import Criterion.Main

main :: IO ()
main = let x1 :: CoRec Identity '[Int,Bool,Char,Double,(),Float]
           x1 = CoRec (Identity (23::Int))
           x5 :: CoRec Identity '[Bool,Char,Double,(),Int,Float]
           x5 = CoRec (Identity (23::Int))
       in defaultMain [ bench "asASafe1" $ whnf (asASafe @Int) x1
                      , bench "asA1" $ whnf (asA @Int) x1
                      , bench "asASafe5" $ whnf (asASafe @Int) x5
                      , bench "asA5" $ whnf (asA @Int) x5 ]
