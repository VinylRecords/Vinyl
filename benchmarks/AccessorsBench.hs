{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Monad (unless)
import           Criterion.Main
import Data.Monoid (Endo(..))
import           Data.Vinyl
import           Data.Vinyl.Syntax ()
import           Lens.Micro        ((%~), (&))
import           System.Exit       (exitFailure)

import           Bench.ARec
import           Bench.SRec
import           Bench.Rec

data HaskRec = HaskRec {
  a0 :: Int,
  a1 :: Int,
  a2 :: Int,
  a3 :: Int,
  a4 :: Int,
  a5 :: Int,
  a6 :: Int,
  a7 :: Int,
  a8 :: Int,
  a9 :: Int,
  a10 :: Int,
  a11 :: Int,
  a12 :: Int,
  a13 :: Int,
  a14 :: Int,
  a15 :: Int  } deriving Show

haskRec :: HaskRec
haskRec = HaskRec 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99

sumHaskRec r =
    a0 r + a1 r + a2 r + a3 r + a4 r + a5 r + a6 r + a7 r + a8 r + a9 r
  + a10 r + a11 r + a12 r + a13 r + a14 r + a15 r

data StrictHaskRec = StrictHaskRec {
  sa0 :: !Int,
  sa1 :: !Int,
  sa2 :: !Int,
  sa3 :: !Int,
  sa4 :: !Int,
  sa5 :: !Int,
  sa6 :: !Int,
  sa7 :: !Int,
  sa8 :: !Int,
  sa9 :: !Int,
  sa10 :: !Int,
  sa11 :: !Int,
  sa12 :: !Int,
  sa13 :: !Int,
  sa14 :: !Int,
  sa15 :: !Int  }

shaskRec :: StrictHaskRec
shaskRec = StrictHaskRec 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99

sumSHaskRec r =
    sa0 r + sa1 r + sa2 r + sa3 r + sa4 r + sa5 r + sa6 r + sa7 r + sa8 r + sa9 r
  + sa10 r + sa11 r + sa12 r + sa13 r + sa14 r + sa15 r

data UStrictHaskRec = UStrictHaskRec {
  usa0 :: {-# UNPACK #-} !Int,
  usa1 :: {-# UNPACK #-} !Int,
  usa2 :: {-# UNPACK #-} !Int,
  usa3 :: {-# UNPACK #-} !Int,
  usa4 :: {-# UNPACK #-} !Int,
  usa5 :: {-# UNPACK #-} !Int,
  usa6 :: {-# UNPACK #-} !Int,
  usa7 :: {-# UNPACK #-} !Int,
  usa8 :: {-# UNPACK #-} !Int,
  usa9 :: {-# UNPACK #-} !Int,
  usa10 :: {-# UNPACK #-} !Int,
  usa11 :: {-# UNPACK #-} !Int,
  usa12 :: {-# UNPACK #-} !Int,
  usa13 :: {-# UNPACK #-} !Int,
  usa14 :: {-# UNPACK #-} !Int,
  usa15 :: {-# UNPACK #-} !Int  }

ushaskRec :: UStrictHaskRec
ushaskRec = UStrictHaskRec 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99

sumUSHaskRec r =
    usa0 r + usa1 r + usa2 r + usa3 r + usa4 r + usa5 r + usa6 r + usa7 r + usa8 r
    + usa9 r + usa10 r + usa11 r + usa12 r + usa13 r + usa14 r + usa15 r

type SubFields = '[ '("a0", Int), '("a8", Int), '("a15", Int)]

-- updateSRec :: forall record. RecordSubset record ElField SubFields Fields
--            => record ElField Fields -> record ElField Fields
updateSRec :: SRec ElField Fields -> SRec ElField Fields
updateSRec = rsubset %~ appEndo aux
  where aux :: Endo (SRec ElField SubFields)
        aux = Endo (\r -> r & #a15 %~ (+ 2) & #a8 %~ (+ 3) & #a0 %~ (+ 4))

updateARec :: ARec ElField Fields -> ARec ElField Fields
updateARec = rsubset %~ appEndo aux
  where aux :: Endo (ARec ElField SubFields)
        aux = Endo (\r -> r & #a15 %~ (+ 2) & #a8 %~ (+ 3) & #a0 %~ (+ 4))

updateRec :: Rec ElField Fields -> Rec ElField Fields
updateRec = rsubset %~ appEndo aux
  where aux :: Endo (Rec ElField SubFields)
        aux = Endo (\r -> r & #a15 %~ (+ 2) & #a8 %~ (+ 3) & #a0 %~ (+ 4))

data SubRec = SubRec { suba0 :: Int, suba8 :: Int, suba15 :: Int }

updateHaskRec :: HaskRec -> HaskRec
updateHaskRec r = r { a0 = suba0 s, a8 = suba8 s, a15 = suba15 s }
  where s = aux (SubRec (a0 r) (a8 r) (a15 r))
        aux r' = r' { suba0 = suba0 r' + 4, suba8 = suba8 r' + 3, suba15 = suba15 r' + 2 }

main :: IO ()
main =
  do let newF = mkRec 0
         arec = toARec newF
         srec = toSRec newF
     unless (rvalf #a15 arec == rvalf #a15 newF)
            (do putStrLn "AFieldRec accessor disagrees with rvalf"
                exitFailure)
     unless (rvalf #a15 srec == rvalf #a15 newF)
            (do putStrLn "SFieldRec accessor disagrees with rvalf"
                exitFailure)
     let srec' = updateSRec srec
         haskRec' = updateHaskRec haskRec
         arec' = updateARec arec
     unless (rvalf #a0 srec' == a0 haskRec' && a0 haskRec' == 4 &&
             rvalf #a8 srec' == a8 haskRec' && a8 haskRec' == 3 &&
             rvalf #a15 srec' == a15 haskRec' && a15 haskRec' == 101)
             (do putStrLn "SRec and Haskell Record updates disagree"
                 exitFailure)
     unless (rvalf #a0 arec' == 4 && rvalf #a8 arec' == 3 &&
             rvalf #a15 arec' == 101)
            (do putStrLn "ARec record updates are inconsistent"
                exitFailure)
     defaultMain
       [ bgroup "Update"
         [ bench "Haskell Record" $ nf (a15 . updateHaskRec) haskRec
         , bench "Rec" $ nf (rvalf #a15 . updateRec) newF
         , bench "ARec" $ nf (rvalf #a15 . updateARec) arec
         , bench "SRec" $ nf (rvalf #a15 . updateSRec) srec
         ]
         ,
         bgroup "creating"
         [ bench "vinyl record" $ whnf mkRec 0
         , bench "toSRec" $ whnf mkToSRec 0
         , bench "New style ARec with toARec " $ whnf mkToARec 0
         , bench "New style ARec with arec " $ whnf mkARec 0
         ]
         ,bgroup "sums"
         [ bench "haskell record" $ nf sumHaskRec haskRec
         , bench "strict haskell record" $ whnf sumSHaskRec shaskRec
         , bench "unboxed strict haskell record" $ whnf sumUSHaskRec ushaskRec
         , bench "vinyl SRec" $ nf sumSRec srec
         , bench "vinyl Rec" $ nf sumRec newF
         , bench "vinyl ARec" $ nf sumARec arec
         ]
       , bgroup "FieldRec"
         [ bench "a0" $ nf (rvalf #a0) newF
         , bench "a4" $ nf (rvalf #a4) newF
         , bench "a8" $ nf (rvalf #a8) newF
         , bench "a12" $ nf (rvalf #a12) newF
         , bench "a15"  $ nf (rvalf #a15) newF
         ]
         , bgroup "AFieldRec"
         [ bench "a0" $ nf (rvalf #a0) arec
         -- , bench "a4" $ nf (rvalf #a4) arec
         -- , bench "a8" $ nf (rvalf #a8) arec
         -- , bench "a12" $ nf (rvalf #a12) arec
         , bench "a15"  $ nf (rvalf #a15) arec
         ]
         , bgroup "SFieldRec"
         [ bench "a0" $ nf (rvalf #a0) srec
         -- , bench "a4" $ nf (rvalf #a4) srec
         -- , bench "a8" $ nf (rvalf #a8) srec
         -- , bench "a12" $ nf (rvalf #a12) srec
         , bench "a15"  $ nf (rvalf #a15) srec
         ]
         , bgroup "Haskell Record"
         [ bench "a0" $ nf a0 haskRec
         -- , bench "a4" $ nf a4 haskRec
         -- , bench "a8" $ nf a8 haskRec
         -- , bench "a12" $ nf a12 haskRec
         , bench "a15"  $ nf a15 haskRec
         ]
         , bgroup "Strict Haskell Record"
         [ bench "a0" $ nf sa0 shaskRec
         -- , bench "a4" $ nf sa4 shaskRec
         -- , bench "a8" $ nf sa8 shaskRec
         -- , bench "a12" $ nf sa12 shaskRec
         , bench "a15"  $ nf sa15 shaskRec
         ]
         , bgroup "Unpacked Strict Haskell Record"
         [ bench "a0" $ nf usa0 ushaskRec
         -- , bench "a4" $ nf usa4 ushaskRec
         -- , bench "a8" $ nf usa8 ushaskRec
         -- , bench "a12" $ nf usa12 ushaskRec
         , bench "a15"  $ nf usa15 ushaskRec
         ]
       ]
