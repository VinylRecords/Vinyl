{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Bench.Rec where

import           Data.Vinyl
import           Data.Vinyl.Syntax ()


type Fields = '[ '( "a0", Int ), '( "a1", Int ), '( "a2", Int ), '( "a3", Int )
               , '( "a4", Int ), '( "a5", Int ), '( "a6", Int ), '( "a7", Int )
               , '( "a8", Int ), '( "a9", Int ), '( "a10", Int ), '( "a11", Int )
               , '( "a12", Int ), '( "a13", Int ), '( "a14", Int ), '( "a15", Int )
               ]

mkRec :: Int -> Rec ElField Fields
mkRec i= Field i :& Field i :& Field i :& Field i :&
         Field i :& Field i :& Field i :& Field i :&
         Field i :& Field i :& Field i :& Field i :&
         Field i :& Field i :& Field i :& Field 99 :&
         RNil

sumRec :: Rec ElField Fields -> Int
sumRec str =
    get #a0 str + get #a1 str + get #a2 str + get #a3 str + get #a4 str
  + get #a5 str + get #a6 str + get #a7 str + get #a8 str
  + get #a9 str + get #a10 str + get #a11 str + get #a12 str
  + get #a13 str + get #a14 str + get #a15 str
  where
    get (_label :: Label s) r =
      let (Field v) = rget @'(s, _) r
      in v
    {-# INLINE get #-}
