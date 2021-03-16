{-# language DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module SupersetSpec (spec) where
import Data.Vinyl
import Data.Vinyl.Functor (Identity(getIdentity))
import Data.Vinyl.Superset
import Test.Hspec (SpecWith, describe, it, shouldBe)

type MyFields = '[Int, String]
type MoreFields = '[Int, String, Float, Bool]

f1 :: r ∈ rs => Rec Identity rs -> r
f1 = getIdentity . rget

f2 :: forall rs. (MyFields ⊆ rs, RPureConstrained (HasElem rs) rs)
   => Rec Identity rs -> String
f2 = onSuperset @String @MyFields f1

spec :: SpecWith ()
spec = do
  describe "Element of subset" $ do
    it "Can see through subset relations" $
      f2 (xrec (23, "Joe", 5.6, False) :: Rec Identity MoreFields) `shouldBe` "Joe"

