{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Bench.SRec where

import Data.Vinyl.SRec
import Data.Vinyl

import Bench.Rec (Fields)


mkToSRec :: Int -> SRec ElField Fields
mkToSRec i= toSRec (Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field 99 :&
                  RNil)


sumSRec :: SRec ElField Fields -> Int
sumSRec str =
    get #a0 str + get #a1 str + get #a2 str + get #a3 str + get #a4 str
  + get #a5 str + get #a6 str + get #a7 str + get #a8 str
  + get #a9 str + get #a10 str + get #a11 str + get #a12 str
  + get #a13 str + get #a14 str + get #a15 str
  where
    get (label :: Label s) r =
      case rget @'(s, Int) r of
        Field v -> v
    {-# INLINE get #-}
