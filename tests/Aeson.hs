{-# LANGUAGE CPP, DataKinds, DeriveGeneric, FlexibleContexts,
             FlexibleInstances, GADTs, OverloadedStrings, PolyKinds,
             ScopedTypeVariables, TypeApplications, TypeOperators,
             ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Demonstrate encoding a 'Rec' to JSON. Three approaches are shown:
-- the first utilizes 'ToJSON' instances for the record's
-- interpretation type constructor applied to each of its fields. This
-- has the advantage of being concise by virtue of re-using a lot of
-- existing pieces. The downside to relying on existing 'ToJSON'
-- instances is that they encode self-contained JSON values, when what
-- we want to do is construct a single JSON object encompassing each
-- record field as a named field of that JSON object. We can do this
-- by inspecting the JSON serialization of each field, and extracting
-- it as a key-value pair if it was serialized as a JSON object with a
-- single named field. This works, but means that the types do not
-- guarantee correctness (i.e. if a record field is serialized as a
-- 'Number', we won't be able to include it in the serialization of
-- the 'Rec').
--
-- The second approach uses a bit of @aeson@ internals to instead
-- serialize each 'Rec' field as a key-value pair with no additional
-- decoration. This should be faster as well as more tightly typed
-- since we do not need to undo any 'Value' wrapping of the individual
-- record fields.
--
-- The third approach uses aeson's built-in functions for working with
-- 'Generic'. This requires some post-processing to address precisely
-- the above problem: the function based on 'Generic' ends up
-- producing a self-contained 'Value' for each field of the
-- record. Specifically, each field becomes an 'Array' that is either
-- empty or contains an 'Object' with a single field as well as
-- another, nested, 'Array' for the rest of the record. We include
-- here a function to flatten that recursive structure into the
-- 'Object' shape we want.
module Main where
import Control.Lens (view, deep)
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as H
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vinyl
import Data.Vinyl.Class.Method (RecMapMethod1(..))
import Data.Vinyl.Functor (Compose(..), (:.), Identity(..), Const(..))
import Data.Aeson
import Data.Aeson.Encoding.Internal (wrapObject, pair)
import Data.Aeson.Lens (_Object)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol)
import Test.Hspec

-- * Implementing 'ToJSON' for 'Rec'

-- | An 'Identity' functor is not reflected in a value's JSON
-- serialization.
instance ToJSON a => ToJSON (Identity a) where
  toJSON (Identity x) = toJSON x

-- | A named field serializes to a JSON object with a single named
-- field.
instance (KnownSymbol s, ToJSON a) => ToJSON (ElField '(s,a)) where
  toJSON x = object [(T.pack (getLabel x), toJSON (getField x))]

-- | A @((Text,) :. f) a@ value maps to a JSON field whose name is the
-- 'Text' value, and whose value has type @f a@.
instance ToJSON (f a) => ToJSON ((((,) Text) :. f) a) where
  toJSON (Compose (name, x)) = object [(name, toJSON x)]

-- | Replace each field of a record with the result of serializing it
-- to a JSON 'Value', and then extracting that 'Value''s single named
-- field. If the serialization is not in the form of an object with a
-- single field, the conversion fails with a 'Nothing'.
fieldsToJSON :: (RecMapMethod1 ToJSON f rs)
             => Rec f rs -> Rec (Maybe :. Const (Text,Value)) rs
fieldsToJSON = rmapMethod1 @ToJSON (Compose . aux)
  where aux x = case toJSON x of
                  Object (H.toList -> [field]) -> Just (Const field)
                  _ -> Nothing

-- | Convert a homogeneous record to a list factored through an outer
-- functor. A useful specialization is when the outer functor is
-- 'Maybe': if any field is 'Nothing', then the result of this
-- function is 'Nothing'.
recToListF :: (Applicative f, RFoldMap rs) => Rec (f :. Const a) rs -> f [a]
recToListF = fmap (rfoldMap (pure . getConst)) . rtraverse getCompose

instance (RFoldMap rs, RecMapMethod1 ToJSON f rs)
  => ToJSON (Rec f rs) where
  toJSON = maybe err object . recToListF . fieldsToJSON
    where err = error (unlines [ "The interpretation functor of this "
                               , "record did not produce a named field "
                               , "for at least one of its fields." ])

-- * Naming anonymous fields

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

-- | A type with its own JSON Object encoding
data MyType = MyType { bike :: Bool, skateboard :: Bool } deriving Generic
instance ToJSON MyType

r3 :: Rec ElField '[ "age" ::: Int
                   , "iscool" ::: Bool
                   , "yearbook" ::: Text
                   , "hobbies" ::: MyType ]
r3 = xrec (23, True, "You spin me right round", MyType True True)

r3JSON :: Value
r3JSON = object [ "age" .= (23 :: Int)
                , "iscool" .= True
                , "yearbook" .= ("You spin me right round" :: Text)
                , "hobbies" .= object ["bike" .= True, "skateboard" .= True] ]

main :: IO ()
main = hspec $ do
  describe "Simple Rec to JSON" $ do
    it "Named fields" $
      toJSON r1 `shouldBe` r1JSON
    it "Anonymous fields" $
      toJSON (nameFields r2) `shouldBe` r2JSON
    it "Nested objects" $
      toJSON r3 `shouldBe` r3JSON
  describe "Type-safe Rec to JSON" $ do
    it "Named fields" $
      recToJSON r1 `shouldBe` r1JSON
    it "Anonymous fields" $
      recToJSON (nameFields r2) `shouldBe` r2JSON
    it "Nested objects" $
      recToJSON r3 `shouldBe` r3JSON
  describe "Via Generics" $ do
    it "Named fields" $
      grecToJSON r1 `shouldBe` r1JSON
    it "Anonymous fields" $
      grecToJSON (nameFields r2) `shouldBe` r2JSON
    it "Nested objects" $
      grecToJSON r3 `shouldBe` r3JSON

-- * More type safe and efficient

-- | Produce a JSON key-value pair from a Haskell value. This is what
-- we want from each field of our records. The simple encoding above
-- that treats each record field as a self-contained JSON 'Value'
-- loses precision in the type.
class ToJSONField a where
  encodeJSONField :: a -> Series
  toJSONField :: a -> (Text,Value)

-- | An @ElField '(s,a)@ value maps to a JSON field with name @s@ and
-- value @a@.
instance (ToJSON a, KnownSymbol s) => ToJSONField (ElField '(s,a)) where
  encodeJSONField x = pair (T.pack (getLabel x)) (toEncoding (getField x))
  toJSONField x = (T.pack (getLabel x), toJSON (getField x))

-- | A @((Text,) :. f) a@ value maps to a JSON field whose name is the
-- 'Text' value, and whose value has type @f a@.
instance ToJSON (f a) => ToJSONField (((,) Text :. f) a) where
  encodeJSONField (Compose (name,val)) = pair name (toEncoding val)
  toJSONField (Compose (name,val)) = (name, toJSON val)

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

-- * Generically

-- | If a 'Value' is a nested 'Array' of 'Object's, extract the
-- collection of key-value pairs from the entire recursive structure.
allAesonFields :: Value -> Maybe (H.HashMap Text Value)
allAesonFields (Array arr) =
  case V.toList arr of
    [] -> Just mempty
    [Object field, objTail] -> fmap (field <>) (allAesonFields objTail)
    _ -> Nothing
allAesonFields _ = Nothing

-- | Try un-nesting a recursive 'Array' of fields. That is, if a
-- 'Value' is laid out as @Array [Object [(key1,value1)], Array
-- [Object [(key2, value2)], ...]]@ we extract all the key-value
-- pairs, @[(key1,value1), (key2, value2), ...]@.
unnestFields :: Value -> Value
unnestFields v = maybe v Object (allAesonFields v)

-- | A lens implementation of something a bit looser than
-- 'unnestFields'.
allFields :: Value -> H.HashMap Text Value
allFields = view (deep _Object)

-- | The generic 'ToJSON' instance is not quite right since we use the
-- record's interpretation type constructor to define serialization,
-- resulting in each record field being treated as a self-contained
-- JSON object. What we want is for each record field to become a
-- named field of a single JSON object, so we must post-process the
-- result of the function defined on 'Generic'.
grecToJSON :: (Generic (Rec f rs), GToJSON Zero (Rep (Rec f rs)))
           => Rec f rs -> Value
grecToJSON = Object . allFields . genericToJSON defaultOptions
