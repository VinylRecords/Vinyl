{-# LANGUAGE PatternSynonyms #-}
module Data.Vinyl
  ( module Data.Vinyl.Core
  , module Data.Vinyl.Class.Method
  , module Data.Vinyl.ARec
  , module Data.Vinyl.Derived
  , module Data.Vinyl.FromTuple
  , module Data.Vinyl.Functor
  , module Data.Vinyl.Lens
  , module Data.Vinyl.SRec
  , module Data.Vinyl.XRec
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Class.Method (RecMapMethod(..), RecPointed(..))
import Data.Vinyl.Class.Method (rmapMethodF, mapFields)
import Data.Vinyl.Class.Method (rtraverseInMethod, rsequenceInFields)
import Data.Vinyl.ARec (ARec, toARec, fromARec)
import Data.Vinyl.Derived
import Data.Vinyl.FromTuple (record, fieldRec, ruple, xrec, xrecX, xrecTuple)
import Data.Vinyl.Functor (ElField(..))
import Data.Vinyl.Lens
import Data.Vinyl.SRec (SRec, toSRec, fromSRec)
import Data.Vinyl.XRec (XRec, pattern (::&), pattern XRNil, IsoXRec(..))
import Data.Vinyl.XRec (xrmap, xrapply, rmapX, XRMap, XRApply)
