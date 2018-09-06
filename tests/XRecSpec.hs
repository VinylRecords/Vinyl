{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications,
             TypeOperators, ViewPatterns #-}
module XRecSpec (spec) where
import Data.Vinyl (Rec, FieldRec, ElField, (:::), (=:))
import Data.Vinyl.FromTuple (namedArgs, ruple, xrec)
import Data.Vinyl.Functor ((:.))
import Data.Vinyl.XRec (rgetX)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | A function that takes named parameters
foo :: FieldRec '["name" ::: String, "age" ::: Int] -> Int
foo (ruple -> (name, age)) = length name + age

spec :: SpecWith ()
spec = do
  describe "Named Arguments" $ do
    it "Can re-order arguments" $
      foo (namedArgs (#age =: (23 :: Int), #name =: "Joe")) `shouldBe` 26
    it "Can pass too many arguments" $
      foo (namedArgs ( #age =: (23 :: Int)
                     , #isAwesome =: True
                     , #name =: "Cheryl"))
      `shouldBe` 29
  describe "Can get fields through HKD" $ do
    let myRec :: Rec (Maybe :. ElField) ["name" ::: String, "age" ::: Int]
        myRec = xrec (Just "joe", Just 23)
    it "Can eliminate Compose newtype wrappers" $ do
      rgetX @("age" ::: Int) myRec `shouldBe` Just 23
