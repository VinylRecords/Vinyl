{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
-- A benchmark where we initialize a 'V.Vector' of random vertices,
-- each carrying 3D position, 2D texture coordinates, and a 3D normal
-- vector. A calculation is carried out where we multiply the y
-- coordinate of each point's normal vector by 2, then sum all normal
-- vector coordinates over all vertices. This calculation is performed
-- by interfacing the vertex data as a flat record, a traditional
-- record of "Linear" finite dimensional vector types, and a vinyl
-- record of linear fields.
import Control.Lens
import Control.Monad (when)
import qualified Data.Foldable as F
import Data.Proxy
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Vinyl
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

vNorm :: Proxy '("normal", V3 a)
vNorm = Proxy

type MyFields a = [ '("pos", V3 a), '("tex", V2 a), '("normal", V3 a) ]
type MyVertex a = FieldRec (MyFields a)

vinylNormSumLens :: (Num a, Storable a) => V.Vector (MyVertex a) -> a
vinylNormSumLens = V.sum . V.map (F.sum . view (rlens vNorm . rfield))

doubleNormYLens :: V.Vector (MyVertex Float) -> V.Vector (MyVertex Float)
doubleNormYLens = V.map (rlens vNorm . rfield . _y *~ (2::Float))

doubleNormY :: V.Vector (MyVertex Float) -> V.Vector (MyVertex Float)
doubleNormY = V.map (\(p :& t :& Field n :& RNil) ->
                       p :& t :& Field (_y *~ (2::Float) $ n) :& RNil)

vinylNormSum :: (Num a, Storable a) => V.Vector (MyVertex a) -> a
vinylNormSum = V.sum . V.map (F.sum . (\(_ :& _ :& Field vn :& RNil) -> vn))

main :: IO ()
main = do vals <- randVecStd $ n * 8 :: IO (V.Vector Float)
          let vinylVerts = V.unsafeCast vals :: V.Vector (MyVertex Float)
              flatVerts = V.unsafeCast vals
              reasVerts = V.unsafeCast vals
              vinylAns = vinylNormSum $ doubleNormY vinylVerts
              vinylLans = vinylNormSumLens $ doubleNormYLens vinylVerts
              flatAns = flatNormSum $ doubleNormFlat flatVerts
              reasAns = reasNormSum $ doubleNormReas reasVerts
          when (any (/= vinylAns) [vinylLans, flatAns, reasAns])
               (error "Not all versions compute the same answer")
          defaultMain [ bench "flat" $
                        whnf (flatNormSum . doubleNormFlat) flatVerts
                      , bench "vinyl" $
                        whnf (vinylNormSum . doubleNormY) vinylVerts
                      , bench "vinyl-lens" $
                        whnf (vinylNormSumLens . doubleNormYLens) vinylVerts
                      , bench "reasonable" $
                        whnf (reasNormSum . doubleNormReas) reasVerts ]
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

flatNormSum :: (Num a, Storable a) => V.Vector (TotallyFlat a) -> a
flatNormSum = V.sum . V.map (\v -> nx v + ny v + nz v)

doubleNormFlat :: V.Vector (TotallyFlat Float) -> V.Vector (TotallyFlat Float)
doubleNormFlat = V.map (\v -> v { ny = ny v * 2 })

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

reasNormSum :: (Num a, Storable a) => V.Vector (Reasonable a) -> a
reasNormSum = V.sum . V.map (F.sum . rNorm)

doubleNormReas :: V.Vector (Reasonable Float) -> V.Vector (Reasonable Float)
doubleNormReas = V.map (\v -> v { rNorm = (_y *~ 2) $ rNorm v })
