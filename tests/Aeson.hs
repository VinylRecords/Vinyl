{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             OverloadedStrings, PolyKinds, ScopedTypeVariables,
             TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vinyl
import Data.Vinyl.Class.Method (RecMapMethod1(..))
import Data.Vinyl.Functor (Compose(..), (:.), Identity(..), Const(..))
import Data.Aeson
import Data.Aeson.Encoding.Internal (wrapObject, pair)
import GHC.TypeLits (KnownSymbol)
import Test.Hspec

-- * Implementing 'ToJSON' for 'Rec'

-- | An 'Identity' functor is not reflected in a value's JSON
-- serialization.
instance ToJSON a => ToJSON (Identity a) where
  toJSON (Identity x) = toJSON x

-- | Produce a JSON key-value pair from a Haskell value.
class ToJSONField a where
  encodeJSONField :: a -> Series
  toJSONField :: a -> (Text,Value)

-- | An @ElField '(s,a)@ value maps to a JSON field with name @s@ and
-- value @a@.
instance (ToJSON a, KnownSymbol s) => ToJSONField (ElField '(s,a)) where
  encodeJSONField x = pair (T.pack (getLabel x)) (toEncoding (getField x))
  toJSONField x = (T.pack (getLabel x), toJSON (getField x))

encodeRec :: (RFoldMap rs, RecMapMethod1 ToJSONField f rs)
          => Rec f rs -> Encoding
encodeRec = wrapObject
          . pairs
          . rfoldMap getConst
          . rmapMethod1 @ToJSONField (Const . encodeJSONField)

recToJSON :: (RFoldMap rs, RecMapMethod1 ToJSONField f rs)
          => Rec f rs -> Value
recToJSON = object
          . rfoldMap ((:[]) . getConst)
          . rmapMethod1 @ToJSONField (Const . toJSONField)

instance (RFoldMap rs, RecMapMethod1 ToJSONField f rs)
  => ToJSON (Rec f rs) where
  toEncoding = encodeRec
  toJSON = recToJSON

-- * Naming anonymous fields

-- | A @((Text,) :. f) a@ value maps to a JSON field whose name is the
-- 'Text' value, and whose value has type @f a@.
instance ToJSON (f a) => ToJSONField (((,) Text :. f) a) where
  encodeJSONField (Compose (name,val)) = pair name (toEncoding val)
  toJSONField (Compose (name,val)) = (name, toJSON val)

-- | Pair each record field with its position.
recIndexed :: Rec f rs -> Rec ((,) Int :. f) rs
recIndexed = flip evalState 1 . rtraverse aux
  where aux x = do i <- get
                   Compose (i,x) <$ put (i+1)

-- | A helper to pair each field of a record with a name derived from
-- its position in the record. This reflects the implicit ordering of
-- the type-level list of the record's fields.
nameFields :: RMap rs => Rec f rs -> Rec ((,) Text :. f) rs
nameFields = rmap aux . recIndexed
  where aux (Compose (i,x)) = Compose ("field"<>T.pack (show i), x)

-- * Test Cases

r1 :: Rec ElField '[("age" ::: Int), ("iscool" ::: Bool), ("yearbook" ::: Text)]
r1 = xrec (23, True, "You spin me right round")

r1JSON :: Value
r1JSON = object [ "age" .= (23 :: Int)
                , "iscool" .= True
                , "yearbook" .= ("You spin me right round" :: Text) ]

r2 :: Rec Identity '[Int,Bool,Text]
r2 = xrec (23, True, "You spin me right round")

r2JSON :: Value
r2JSON = object [ "field1" .= (23 :: Int)
                , "field2" .= True
                , "field3" .= ("You spin me right round" :: Text) ]

main :: IO ()
main = hspec $ do
  describe "Encode Rec to JSON" $ do
    it "Named fields" $
      toJSON r1 `shouldBe` r1JSON
    it "Anonymous fields" $
      toJSON (nameFields r2) `shouldBe` r2JSON
