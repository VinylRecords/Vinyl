{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables,
             TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
import Data.Vinyl
import Data.Vinyl.Functor (Lift(..), Const(..), Compose(..), (:.))
import Test.Hspec

import qualified CoRecSpec as C

-- d1 :: FieldRec '[ '("X",String), '("Y", String) ]
-- d1 = Field @"X" "5" :& Field @"Y" "Hi" :& RNil

-- d2 :: FieldRec '[ '("X", String -> Int), '("Y", String -> String) ]
-- d2 = Field @"X" (read :: String -> Int)
--      :& Field @"Y" (id :: String -> String)
--      :& RNil

d1' :: Rec (Const String) '[ '("X", Int), '("Y", String) ]
d1' = Const "5" :& Const "Hi" :& RNil

d2' :: Rec ((->) String :. ElField) '[ '("X", Int), '("Y", String) ]
d2' = Compose (Field . read) :& Compose (Field . id) :& RNil

d3 :: Rec ElField '[ '("X", Int), '("Y", String) ]
d3 = rmap (\(Compose f) -> Lift (f . getConst)) d2' <<*>> d1'

main :: IO ()
main = hspec $ do
  C.spec
  describe "Rec is like an Applicative" $ do
    it "Can apply parsing functions" $ d3 `shouldBe` Field 5 :& Field "Hi" :& RNil
