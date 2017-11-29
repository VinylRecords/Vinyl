{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

import           Criterion.Main
import           Data.Tagged
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

{-# INLINE getF #-}
getF :: RElem '(s, t) rs (RIndex '(s, t) rs) => sing '(s, t) -> FieldRec rs -> t
getF l = getField . rget l

newF :: FieldRec '[ '( "a0", Int ), '( "a1", Int ), '( "a2", Int ), '( "a3", Int )
                  , '( "a4", Int ), '( "a5", Int ), '( "a6", Int ), '( "a7", Int )
                  , '( "a8", Int ), '( "a9", Int ), '( "a10", Int ), '( "a11", Int )
                  , '( "a12", Int ), '( "a13", Int ), '( "a14", Int ), '( "a15", Int )
                  ]
newF = Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 0 :&
       Field 0 :& Field 0 :& Field 0 :& Field 0 :&
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
main = defaultMain
  [ bgroup "FieldRec"
    [ bench "a0" $ nf (getF @"a0" @Int SField) newF
    , bench "a4" $ nf (getF @"a4" @Int SField) newF
    , bench "a8" $ nf (getF @"a8" @Int SField) newF
    , bench "a12" $ nf (getF @"a12" @Int SField) newF
    , bench "a15"  $ nf (getF @"a15" @Int SField) newF
    ]
    , bgroup "HList"
    [ bench "a0" $ nf (getH @"a0" @Int SField) newH
    , bench "a4" $ nf (getH @"a4" @Int SField) newH
    , bench "a8" $ nf (getH @"a8" @Int SField) newH
    , bench "a12" $ nf (getH @"a12" @Int SField) newH
    , bench "a15" $ nf (getH @"a15" @Int SField) newH
    ]
  ]
