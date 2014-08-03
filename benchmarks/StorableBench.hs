{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, PolyKinds #-}
-- A benchmark where we initialize a 'V.Vector' of random vertices,
-- each carrying 3D position, 2D texture coordinates, and a 3D normal
-- vector. A calculation is carried out where we multiply the y
-- coordinate of each point's normal vector by 2, then sum all normal
-- vector coordinates over all vertices. This calculation is performed
-- by interfacing the vertex data as a flat record, a traditional
-- record of "Linear" finite dimensional vector types, and a vinyl
-- record of linear fields.
import Control.Applicative
import Control.Lens
import Control.Monad (when)
import qualified Data.Foldable as F
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Vinyl
import Data.Vinyl.Universe.Field
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Linear (V2, V3, _y)
import System.Random.MWC (withSystemRandom, Variate(..), GenIO)
import Criterion.Main

randVec :: (Storable a, Variate a) => Int -> GenIO -> IO (V.Vector a)
randVec n g = VM.replicateM n (uniform g) >>=
              V.unsafeFreeze

randVecStd :: (Storable a, Variate a) => Int -> IO (V.Vector a)
randVecStd = withSystemRandom . randVec

vNorm :: SField ("normal" ::: V3 a)
vNorm = SField

type MyFields a = [ "pos" ::: V3 a, "tex" ::: V2 a, "normal" ::: V3 a ]
type MyVertex a = PlainRec ElField (MyFields a)

doubleNviL :: V.Vector (MyVertex Float) -> V.Vector (MyVertex Float)
doubleNviL = V.map (rlens vNorm . _y *~ (2::Float))

vinylNSumL :: (Num a, Storable a) => V.Vector (MyVertex a) -> a
vinylNSumL = V.sum . V.map (F.sum . view (rlens vNorm))

doubleNvi :: V.Vector (MyVertex Float) -> V.Vector (MyVertex Float)
doubleNvi = V.map (rMod vNorm (_y *~ (2::Float)))

vinylNSum :: (Num a, Storable a) => V.Vector (MyVertex a) -> a
vinylNSum = V.sum . V.map (F.sum . rGet vNorm)

main :: IO ()
main = do vals <- randVecStd $ n * 8 :: IO (V.Vector Float)
          let vinylVerts = V.unsafeCast vals :: V.Vector (MyVertex Float)
              flatVerts = V.unsafeCast vals
              reasVerts = V.unsafeCast vals
              vinylAns = vinylNSum $ doubleNvi vinylVerts
              vinylLans = vinylNSumL $ doubleNviL vinylVerts
              flatAns = flatNSum $ doubleNfl flatVerts
              reasAns = reasNSum $ doubleNre reasVerts
          when (any (/= vinylAns) [vinylLans, flatAns, reasAns])
               (error "Not all versions compute the same answer")
          defaultMain [ bench "flat" $ whnf (flatNSum . doubleNfl) flatVerts
                      , bench "vinyl" $ whnf (vinylNSum . doubleNvi) vinylVerts
                      , bench "vinyl-lens" $ whnf (vinylNSumL . doubleNviL) vinylVerts
                      , bench "reasonable" $
                        whnf (reasNSum . doubleNre) reasVerts ]
  where n = 1000

--------------------------------------------------------------------------------
-- Baseline data structures for comparison

-- Don't trust data structures at all? Use a flat record where only
-- the field prefixes denote their roles.
data TotallyFlat a = Flat { px :: !a
                          , py :: !a
                          , pz :: !a
                          , tu :: !a
                          , tv :: !a
                          , nx :: !a
                          , ny :: !a
                          , nz :: !a }

instance Storable a => Storable (TotallyFlat a) where
  sizeOf _ = sizeOf (undefined::a) * 8
  alignment _ = alignment (undefined::a)
  peek ptr = Flat <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
                  <*> peekElemOff ptr' 3 <*> peekElemOff ptr' 4
                  <*> peekElemOff ptr' 5 <*> peekElemOff ptr' 6
                  <*> peekElemOff ptr' 7
    where ptr' = castPtr ptr
  poke ptr (Flat px' py' pz' tu' tv' nx' ny' nz') = do poke ptr' px'
                                                       pokeElemOff ptr' 1 py'
                                                       pokeElemOff ptr' 2 pz'
                                                       pokeElemOff ptr' 3 tu'
                                                       pokeElemOff ptr' 4 tv'
                                                       pokeElemOff ptr' 5 nx'
                                                       pokeElemOff ptr' 6 ny'
                                                       pokeElemOff ptr' 7 nz'
    where ptr' = castPtr ptr

flatNSum :: (Num a, Storable a) => V.Vector (TotallyFlat a) -> a
flatNSum = V.sum . V.map (\v -> nx v + ny v + nz v)

doubleNfl :: V.Vector (TotallyFlat Float) -> V.Vector (TotallyFlat Float)
doubleNfl = V.map (\v -> v { ny = ny v * 2 })

-- A more reasonable approach to a vertex record.
data Reasonable a = Reasonable { rPos  :: V3 a
                               , rTex  :: V2 a
                               , rNorm :: V3 a }

instance Storable a => Storable (Reasonable a) where
  sizeOf _ = sizeOf (undefined::V3 a)*2 + sizeOf (undefined::V2 a)
  alignment _ = alignment (undefined::V3 a)
  peek ptr = Reasonable <$> peek (castPtr ptr)
                        <*> peekByteOff (castPtr ptr) szx
                        <*> peekByteOff (castPtr ptr) (szx + szy)
    where szx = sizeOf (undefined::V3 a)
          szy = sizeOf (undefined::V2 a)
  poke ptr (Reasonable p t n) = do poke (castPtr ptr) p
                                   pokeByteOff (castPtr ptr) szx t
                                   pokeByteOff (castPtr ptr) (szx + szy) n
    where szx = sizeOf (undefined::V3 a)
          szy = sizeOf (undefined::V2 a)

reasNSum :: (Num a, Storable a) => V.Vector (Reasonable a) -> a
reasNSum = V.sum . V.map (F.sum . rNorm)

doubleNre :: V.Vector (Reasonable Float) -> V.Vector (Reasonable Float)
doubleNre = V.map (\v -> v { rNorm = (_y *~ 2) $ rNorm v })
