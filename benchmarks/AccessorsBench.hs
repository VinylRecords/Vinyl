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

{-# INLINE getH #-}
getH :: forall t r sing rs . RElem (Tagged t r) rs (RIndex (Tagged t r) rs) => sing t -> HList rs -> r
getH _ = untag . getIdentity . rget (SField :: SField (Tagged t r))

newH :: HList '[ Tagged "a0" Int, Tagged  "a1" Int,  Tagged  "a2" Int,  Tagged  "a3" Int
                 , Tagged "a4" Int, Tagged  "a5" Int,  Tagged  "a6" Int,  Tagged  "a7" Int
                 , Tagged "a8" Int, Tagged  "a9" Int,  Tagged  "a10" Int,  Tagged  "a11" Int
                 , Tagged "a12" Int, Tagged  "a13" Int,  Tagged  "a14" Int,  Tagged  "a15" Int
                 ]
newH = Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :&
       Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :&
       Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :&
       Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :& Identity (Tagged 0) :&
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
         , bgroup "FieldRec Storable"
         [ bench "a0" $ nfIO (fmap (rvalf #a0) . FS.peek $ ptr)
         , bench "a4" $ nfIO (fmap (rvalf #a4) . FS.peek $ ptr)
         , bench "a8" $ nfIO (fmap (rvalf #a8) . FS.peek $ ptr)
         , bench "a12" $ nfIO (fmap (rvalf #a12) . FS.peek $ ptr)
         , bench "a15"  $ nfIO (fmap (rvalf #a15) . FS.peek $ ptr)
         ]
         , bgroup "AFieldRec"
         [ bench "a0" $ nf (rvalf #a0) arec
         , bench "a4" $ nf (rvalf #a4) arec
         , bench "a8" $ nf (rvalf #a8) arec
         , bench "a12" $ nf (rvalf #a12) arec
         , bench "a15"  $ nf (rvalf #a15) arec
         ]
         , bgroup "HList"
         [ bench "a0" $ nf (getH @"a0" @Int SField) newH
         , bench "a4" $ nf (getH @"a4" @Int SField) newH
         , bench "a8" $ nf (getH @"a8" @Int SField) newH
         , bench "a12" $ nf (getH @"a12" @Int SField) newH
         , bench "a15" $ nf (getH @"a15" @Int SField) newH
         ]
       ]
