{-# LANGUAGE DataKinds, FlexibleContexts, GADTs,
             NoMonomorphismRestriction, OverloadedLabels,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}

module Test.ARec where

import Data.Vinyl.ARec
import Data.Vinyl
import Test.Hspec

import Data.Vinyl.Syntax ()

type FullARec = ARec ElField '[ "f0" ::: Int , "f1" ::: Bool , "f2" ::: String
                              , "f3" ::: Double, "f4" ::: Integer
                              , "f2" ::: Int -- intentionally duplicate field name
                              ]

type SubARecPre = ARec ElField '[ "f0" ::: Int , "f1" ::: Bool , "f2" ::: String ]

type SubARecDupes = ARec ElField '[ "f2" ::: String, "f2" ::: String
                                  , "f2" ::: Int, "f2" ::: String
                                  ]


fullARec :: FullARec
fullARec = toARec ( #f0 =: 1 :& #f1 =: False :& #f2 =: "field2"
                  :& #f3 =: 3.1415 :& #f4 =: 4444
                  :& #f2 =: 666
                  :& RNil
                  )

-- For arecGetSubset -----------------------------------------------------------

subARecPre :: SubARecPre
subARecPre = toARec ( #f0 =: 1 :& #f1 =: False :& #f2 =: "field2" :&  RNil)

subARecDupes :: SubARecDupes
subARecDupes = toARec ( #f2 =: "field2" :& #f2 =: "field2"
                        :& #f2 =: 666 :& #f2 =: "field2"
                        :& RNil
                      )

arecWithDupes :: ARec ElField '[ "f" ::: Int, "f" ::: Int]
arecWithDupes = toARec (#f =: 1 :& #f =: 2 :& RNil)

-- For arecSetSubset -----------------------------------------------------------

subARecPreSet :: SubARecPre
subARecPreSet = toARec ( #f0 =: 11 :& #f1 =: True :& #f2 =: "field2-updated" :&  RNil)

fullARecUpdated :: FullARec
fullARecUpdated = toARec ( #f0 =: 11 :& #f1 =: True :& #f2 =: "field2-updated"
                           :& #f3 =: 3.1415 :& #f4 =: 4444
                           :& #f2 =: 666
                           :& RNil
                         )

updateARecWithDupes :: ARec ElField '[ '("f0", Int), '("f0", Int), '("f0", Int)]
updateARecWithDupes = toARec (#f0 =: 3 :& #f0 =: 66 :& #f0 =: 1 :&RNil)

subARecDupesUpdated :: SubARecDupes
subARecDupesUpdated = toARec ( #f2 =: "updated" :& #f2 =: "field2"
                               :& #f2 =: 666 :& #f2 =: "field2"
                               :& RNil
                             )



spec :: SpecWith ()
spec = describe "ARec" $ do
  describe "arecGetSubset" $ do
    it "retrieves a prefix ARec" $
      -- The part to be retrieved is type-directed
      arecGetSubset fullARec `shouldBe` subARecPre
    it "retrieves the full ARec" $ do
      -- Should catch off-by-one errors that lead to overflow
      arecGetSubset fullARec `shouldBe` fullARec
    it "handles an empty subARec correctly" $
      arecGetSubset fullARec `shouldBe` toARec RNil
    it "handles duplicate field names correctly in the sub arec" $
      arecGetSubset fullARec `shouldBe` subARecDupes
    it "handles duplicate field names correctly in the source arec" $
      -- When both the name and the type of the field match we retrieve from the
      -- first field
      arecGetSubset arecWithDupes `shouldBe` toARec (#f =: (1 :: Int) :& RNil)
  describe "arecSetSubset" $ do
    it "sets a subset of fields" $ do
      arecSetSubset fullARec subARecPreSet `shouldBe` fullARecUpdated
    it "handles updates to every field" $ do
      -- Should catch off-by-one errors that lead to overflow
      arecSetSubset fullARec fullARec `shouldBe` fullARec
    it "handles an empty subset" $ do
      arecSetSubset fullARec (toARec RNil) `shouldBe` fullARec
    it "handles duplicates in the updating ARec" $ do
      -- The behaviour here should be that the _last_ updating field prevails
      arecSetSubset fullARec updateARecWithDupes `shouldBe` fullARec
    it "handles updatees with duplicate fields" $ do
      -- Here, only the _first_ field should be updated
      arecSetSubset subARecDupes (toARec (#f2 =: "updated" :& RNil))
        `shouldBe` subARecDupesUpdated
