{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedLabels      #-}

import           Control.Monad (unless)
import           Criterion.Main
import           Data.Vinyl
import           System.Exit (exitFailure)

newF :: FieldRec '[ '( "a0", Int ), '( "a1", Int ), '( "a2", Int ), '( "a3", Int )
                  , '( "a4", Int ), '( "a5", Int ), '( "a6", Int ), '( "a7", Int )
                  , '( "a8", Int ), '( "a9", Int ), '( "a10", Int ), '( "a11", Int )
                  , '( "a12", Int ), '( "a13", Int ), '( "a14", Int ), '( "a15", Int )
                  ]
newF = Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 99 :&
       RNil

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
  a15 :: Int  }

haskRec :: HaskRec
haskRec = HaskRec 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99

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

main :: IO ()
main =
  do let arec = toARec newF
         srec = toSRec newF
     unless (rvalf #a15 arec == rvalf #a15 newF)
            (do putStrLn "AFieldRec accessor disagrees with rvalf"
                exitFailure)
     unless (rvalf #a15 srec == fieldGet)
            (do putStrLn "SFieldRec accessor disagrees with rvalf"
                exitFailure)
     defaultMain
       [ bgroup "FieldRec"
         [ bench "a0" $ nf (rvalf #a0) newF
         , bench "a4" $ nf (rvalf #a4) newF
         , bench "a8" $ nf (rvalf #a8) newF
         , bench "a12" $ nf (rvalf #a12) newF
         , bench "a15"  $ nf (rvalf #a15) newF
         ]
         , bgroup "AFieldRec"
         [ bench "a0" $ nf (rvalf #a0) arec
         , bench "a4" $ nf (rvalf #a4) arec
         , bench "a8" $ nf (rvalf #a8) arec
         , bench "a12" $ nf (rvalf #a12) arec
         , bench "a15"  $ nf (rvalf #a15) arec
         ]
         , bgroup "FieldRec Storable"
         [ bench "a0" $ nfIO (fmap (rvalf #a0) . FS.peek $ ptr)
         , bench "a4" $ nfIO (fmap (rvalf #a4) . FS.peek $ ptr)
         , bench "a8" $ nfIO (fmap (rvalf #a8) . FS.peek $ ptr)
         , bench "a12" $ nfIO (fmap (rvalf #a12) . FS.peek $ ptr)
         , bench "a15"  $ nfIO (fmap (rvalf #a15) . FS.peek $ ptr)
         ]
         , bgroup "SFieldRec"
         [ bench "a0" $ nf (rvalf #a0) srec
         , bench "a4" $ nf (rvalf #a4) srec
         , bench "a8" $ nf (rvalf #a8) srec
         , bench "a12" $ nf (rvalf #a12) srec
         , bench "a15"  $ nf (rvalf #a15) srec
         ]
         , bgroup "Haskell Record"
         [ bench "a0" $ nf a0 haskRec
         , bench "a4" $ nf a4 haskRec
         , bench "a8" $ nf a8 haskRec
         , bench "a12" $ nf a12 haskRec
         , bench "a15"  $ nf a15 haskRec
         ]
         , bgroup "Strict Haskell Record"
         [ bench "a0" $ nf sa0 shaskRec
         , bench "a4" $ nf sa4 shaskRec
         , bench "a8" $ nf sa8 shaskRec
         , bench "a12" $ nf sa12 shaskRec
         , bench "a15"  $ nf sa15 shaskRec
         ]
         , bgroup "Unpacked Strict Haskell Record"
         [ bench "a0" $ nf usa0 ushaskRec
         , bench "a4" $ nf usa4 ushaskRec
         , bench "a8" $ nf usa8 ushaskRec
         , bench "a12" $ nf usa12 ushaskRec
         , bench "a15"  $ nf usa15 ushaskRec
         ]
       ]
