{-# LANGUAGE CPP, DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables,
             TypeOperators #-}
#if __GLASGOW_HASKELL__ > 800
{-# LANGUAGE OverloadedLabels #-}
#endif

{-# OPTIONS_GHC -Wall #-}
import Data.Vinyl
import Data.Vinyl.Functor (Lift(..), Const(..), Compose(..), (:.))
import Lens.Micro
import Test.Hspec

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
#if __GLASGOW_HASKELL__ > 800
  describe "Fields may be accessed by overloaded labels" $ do
    it "Can get field X" $ rvalf #x d3 `shouldBe` 5
    it "Can get field Y" $ rvalf #y d3 `shouldBe` "Hi"
  describe "ARec provides field accessors" $ do
    it "Can get field Y" $ rvalf #y (toARec d3) `shouldBe` "Hi"
    it "Can set field X" $ rvalf #x (rputf #x 7 (toARec d3)) `shouldBe` 7
  describe "Converting between Rec and ARec" $ do
    it "Can go back and forth" $
      rvalf #y (toARec (rlensf #y %~ (show . length) $
                            fromARec (rputf #x 7 (toARec d3))))
      `shouldBe` "2"
#endif
