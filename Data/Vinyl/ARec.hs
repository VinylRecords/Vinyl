{-# LANGUAGE Trustworthy #-}

-- | Constant-time field accessors for extensible records. The
-- trade-off is the usual lists vs arrays one: it is fast to add an
-- element to the head of a list, but element access is linear time;
-- array access time is uniform, but extending the array is more
-- slower.
module Data.Vinyl.ARec
  ( ARec -- Exported abstractly
  , IndexableField
  , toARec
  , fromARec
  , aget
  , aput
  , alens
  , arecGetSubset
  , arecSetSubset
  ) where
import Data.Vinyl.ARec.Internal
