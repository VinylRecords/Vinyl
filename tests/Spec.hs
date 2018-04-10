{-# LANGUAGE DataKinds, FlexibleContexts, GADTs,
             NoMonomorphismRestriction, OverloadedLabels,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
import Data.Vinyl
import Data.Vinyl.Functor (Lift(..), Const(..), Compose(..), (:.))
import Lens.Micro
import Test.Hspec
import Data.Vinyl.Syntax ()

import qualified CoRecSpec as C

-- d1 :: FieldRec '[ '("X",String), '("Y", String) ]
-- d1 = Field @"X" "5" :& Field @"Y" "Hi" :& RNil

-- d2 :: FieldRec '[ '("X", String -> Int), '("Y", String -> String) ]
-- d2 = Field @"X" (read :: String -> Int)
--      :& Field @"Y" (id :: String -> String)
--      :& RNil

d1' :: Rec (Const String) '[ '("x", Int), '("y", String) ]
d1' = Const "5" :& Const "Hi" :& RNil

d2' :: Rec ((->) String :. ElField) '[ '("x", Int), '("y", String) ]
d2' = Compose (Field . read) :& Compose (Field . id) :& RNil

d3 :: Rec ElField '[ '("x", Int), '("y", String) ]
d3 = rmap (\(Compose f) -> Lift (f . getConst)) d2' <<*>> d1'

main :: IO ()
main = hspec $ do
  C.spec
  describe "Rec is like an Applicative" $ do
    it "Can apply parsing functions" $ d3 `shouldBe` Field 5 :& Field "Hi" :& RNil
  describe "Fields may be accessed by overloaded labels" $ do
    it "Can get field X" $ rvalf #x d3 `shouldBe` 5
    it "Can get field Y" $ rvalf #y d3 `shouldBe` "Hi"
  describe "ARec provides field accessors" $ do
    it "Can get field Y" $ rvalf #y (toARec d3) `shouldBe` "Hi"
    it "Can set field X" $ rvalf #x (rputf #x 7 (toARec d3)) `shouldBe` 7
  describe "Converting between Rec and ARec" $ do
    it "Can go back and forth" $
      rvalf #y (toARec (#y %~ (show . length) $
                          fromARec (rputf #x 7 (toARec d3))))
      `shouldBe` "2"
  describe "Converting between Rec and SRec" $ do
    it "Can go back and forth" $
      let d4 = #x =:= 5 <+> #y =:= 4 :: FieldRec '[ '("x",Int), '("y",Int)]
          isqrt = floor . (sqrt :: Double -> Double) . fromIntegral :: Int -> Int
      in rvalf #y (toSRec (#y %~ isqrt $
           fromSRec (rputf #x 7 (toSRec d4))))
      `shouldBe` 2

  describe "Produces field lenses from overloaded labels" $ do
    it "Can invert a boolean field" $ do
      (fieldRec (#x =: True, #y =: 'b') & #x %~ not)
      `shouldBe` fieldRec (#x =: False, #y =: 'b')
  describe "Supports tuple construction" $ do
    it "Can build ElField records from tuples" $
          fieldRec (#x =: 5, #y =: "Hi") `shouldBe` d3
    it "Can build Recs of Maybe values" $
      record @Maybe (Just True, Just 'a') `shouldBe` Just True :& Just 'a' :& RNil
    it "Can build Recs of Const values" $
      record @(Const String) ( Const "howdy" :: Const String Int
                             , Const "folks" :: Const String Double)
      `shouldBe` Const "howdy" :& Const "folks" :& RNil
  describe "Can change the types of individual fields" $ do
    it "Can set a field with a different type" $
      (#x .~ 2.1) d3 `shouldBe` fieldRec (#x =: 2.1, #y =: "Hi")
    it "Can change a field's type" $
      (d3 & #y %~ length) `shouldBe` fieldRec (#x =: 5, #y =: 2)
