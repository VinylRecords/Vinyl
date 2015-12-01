{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}
import Control.Monad (join)
import Criterion.Main
import Data.Functor.Identity
import Data.Vinyl
import Data.Vinyl.TypeLevel

class Eq2 a where
  eq2 :: a -> a -> Bool

instance RecAll f rs Eq => Eq2 (Rec f rs) where
  eq2 RNil RNil = True
  eq2 (a :& as) (b :& bs) = a == b && eq2 as bs

main :: IO ()
main = defaultMain [
         bench "Eq" $ whnf (join (==)) r1
       , bench "Eq2" $ whnf (join eq2) r1 ]
  where r1 = pure 23 :& pure 'b' :& pure 3.14 :& RNil :: Rec Identity '[Int, Char, Double]
