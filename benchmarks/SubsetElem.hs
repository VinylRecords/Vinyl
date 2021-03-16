{-# language DataKinds, FlexibleContexts, TypeApplications, TypeOperators #-}
import Data.Vinyl
import Data.Vinyl.Functor (Identity(getIdentity))
import Data.Vinyl.Superset
import Criterion.Main

type MyFields = '[Int, String]
type MoreFields = '[Int, String, Float, Bool]

f1 :: r ∈ rs => Rec Identity rs -> r
f1 = getIdentity . rget

main :: IO ()
main =
  let r1 = xrec (23, "Joe") :: Rec Identity MyFields
      r2 = xrec (23, "Joe", 5.6, False) :: Rec Identity MoreFields
  in defaultMain [ bench "baseline get" $ whnf (f1 @String) r1
                 , bench "subset get" $
                   whnf (onSuperset @String @MyFields (f1 @String)) r2
                 , bench "rcast get" $ whnf (f1 @String . rcast @MyFields) r2 ]
