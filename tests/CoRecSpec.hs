{-# LANGUAGE CPP, DataKinds, FlexibleContexts, ScopedTypeVariables,
             TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module CoRecSpec (spec) where
import Control.Monad ((>=>))
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.CoRec
import Data.Vinyl.Functor (Identity(..))

import Test.Hspec
import Test.ShouldNotTypecheck

-- Custom error types
data TooBig = TooBig
data Even = Even
data Not7 = Not7

-- Functions that might return an error value
fun1 :: (TooBig ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun1 x = if x < 10 then Right () else Left (CoRec (pure TooBig))

fun2 :: (Even ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun2 x = if odd x then Right () else Left (CoRec (pure Even))

fun3 :: (Not7 ∈ rs) => Int -> Either (CoRec Identity rs) ()
fun3 x = if x == 7 then Right () else Left (CoRec (pure Not7))

spec :: SpecWith ()
spec = do
  describe "CoRecs" $ do
    let x = CoRec (pure True) :: Field '[Int,Bool,()]
    it "Can be cast successfully" $
      asA @Bool x `shouldBe` Just True
    it "Can fail to cast" $
      asA @Int x `shouldBe` Nothing
    it "Can be handled all at once" $
      match x (H (\y -> "Int")
               :& H (\y -> "Bool")
               :& H (\y -> "Unit")
               :& RNil) `shouldBe` "Bool"
    it "Can be handled piece by piece, out of order" $
      let handlers = match1 (H (\(u :: ()) -> "unit"))
                     >=> match1 (H (\(b :: Bool) -> "bool "++show b))
                     >=> match1 (H (\(i :: Int) ->  "int "++show i))
      in either id matchNil (handlers x) `shouldBe` "bool True"
    it "Can detect partial pattern matches" $
      let handlers = match1 (H (\(u :: ()) -> "unit"))
                     >=> match1 (H (\(b :: Bool) -> "bool "++show b))
      in shouldNotTypecheck (either id matchNil (handlers x))
