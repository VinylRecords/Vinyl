{-# LANGUAGE DataKinds, OverloadedLabels, TypeOperators, ViewPatterns #-}
module XRecSpec (spec) where
import Data.Vinyl (FieldRec, (:::), (=:))
import Data.Vinyl.FromTuple (namedArgs, ruple)
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
