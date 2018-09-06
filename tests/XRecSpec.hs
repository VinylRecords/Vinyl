{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedLabels,
             TypeApplications, TypeOperators, ViewPatterns #-}
module XRecSpec (spec) where
import Data.Vinyl
import Data.Vinyl.FromTuple (namedArgs, ruple, xrec, fieldRec, withDefaults)
import Data.Vinyl.Functor ((:.))
import Data.Vinyl.XRec (rgetX)
import Test.Hspec (SpecWith, describe, it, shouldBe)

-- | A function that takes named parameters.
foo :: FieldRec '["name" ::: String, "age" ::: Int] -> Int
foo (ruple -> (name, age)) = length name + age

-- | Like 'foo', but has default values for each parameter.
foo' :: (RMap ss, ss ⊆ '["name" ::: String, "age" ::: Int])
     => Rec ElField ss -> Int
foo' = foo . withDefaults defs
  where defs = fieldRec (#name =: "roberta", #age =: (48 :: Int))

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

  describe "Default arguments" $ do
    it "Can take zero arguments" $
      foo' (fieldRec ()) `shouldBe` 55
    it "Can take a subset of named arguments (1)" $
      foo' (#age =:= (39::Int)) `shouldBe` 46
    it "Can take a subset of named arguments (2)" $
      foo' (#name =:= "Jerome") `shouldBe` 54
    it "Can take all arguments" $
      foo' (fieldRec (#age =: (36::Int), #name =: "Jerome")) `shouldBe` 42

  describe "Can get fields through HKD" $ do
    let myRec :: Rec (Maybe :. ElField) ["name" ::: String, "age" ::: Int]
        myRec = xrec (Just "joe", Just 23)
    it "Can eliminate Compose newtype wrappers" $ do
      rgetX @("age" ::: Int) myRec `shouldBe` Just 23
