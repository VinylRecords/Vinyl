{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Monad (unless)
import           Criterion.Main
import           Data.Tagged
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import qualified Foreign.Marshal.Alloc as Ptr
import qualified Foreign.Storable as FS
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

main :: IO ()
main =
  do ptr <- Ptr.malloc
     FS.poke ptr newF
     let fieldGet = rvalf #a15 newF
     fieldPeek <- rvalf #a15 <$> FS.peek ptr
     unless (fieldGet == fieldPeek)
            (do putStrLn "Storable field accessor disagrees with rvalf"
                exitFailure)
     let arec = toARec newF
     unless (rvalf #a15 arec == fieldGet)
            (do putStrLn "AFieldRec accessor disagrees with rvalf"
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
       ]
