{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Bench.ARec where

import Data.Vinyl
import Data.Vinyl.ARec.Internal
import Data.Vinyl.Syntax ()

import Bench.Rec

mkARec :: Int -> ARec ElField Fields
mkARec i= arec (Field i `arcons` Field i `arcons` Field i `arcons` Field i `arcons`
                  Field i `arcons` Field i `arcons` Field i `arcons` Field i `arcons`
                  Field i `arcons` Field i `arcons` Field i `arcons` Field i `arcons`
                  Field i `arcons` Field i `arcons` Field i `arcons` Field 99 `arcons`
                  arnil)


mkToARec :: Int -> ARec ElField Fields
mkToARec i= toARec (Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field i :&
                  Field i :& Field i :& Field i :& Field 99 :&
                  RNil)

sumARec :: ARec ElField Fields -> Int
sumARec str =
    get #a0 str + get #a1 str + get #a2 str + get #a3 str + get #a4 str
  + get #a5 str + get #a6 str + get #a7 str + get #a8 str
  + get #a9 str + get #a10 str + get #a11 str + get #a12 str
  + get #a13 str + get #a14 str + get #a15 str
  where
    get label r = rvalf label r
    {-# INLINE get #-}
