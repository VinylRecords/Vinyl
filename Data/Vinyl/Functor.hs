{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Vinyl.Functor
  ( -- * Introduction
    -- $introduction
    -- * Data Types
    Identity(..)
  , Thunk(..)
  , Lift(..)
  , ElField(..)
  , Compose(..), onCompose
  , (:.)
  , Const(..)
    -- * Discussion

    -- ** Example
    -- $example

    -- ** Ecosystem
    -- $ecosystem
  ) where

import Data.Proxy
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits
import GHC.Types (Type)
import Data.Vinyl.TypeLevel (Snd)

{- $introduction
    This module provides functors and functor compositions
    that can be used as the interpretation function for a
    'Rec'. For a more full discussion of this, scroll down
    to the bottom.
-}

-- | This is identical to the "Identity" from "Data.Functor.Identity"
-- in "base" except for its 'Show' instance.
newtype Identity a
  = Identity { getIdentity :: a }
    deriving ( Functor
             , Foldable
             , Traversable
             , Storable
             , Eq
             , Ord
             , Generic
             )

-- | Used this instead of 'Identity' to make a record
--   lazy in its fields.
data Thunk a
  = Thunk { getThunk :: a }
    deriving ( Functor
             , Foldable
             , Traversable
             )

newtype Lift (op :: l -> l' -> *) (f :: k -> l) (g :: k -> l') (x :: k)
  = Lift { getLift :: op (f x) (g x) }

newtype Compose (f :: l -> *) (g :: k -> l) (x :: k)
  = Compose { getCompose :: f (g x) }
    deriving (Storable, Generic)

instance Semigroup (f (g a)) => Semigroup (Compose f g a) where
  Compose x <> Compose y = Compose (x <> y)

instance Monoid (f (g a)) => Monoid (Compose f g a) where
  mempty = Compose mempty
  mappend (Compose x) (Compose y) = Compose (mappend x y)

-- | Apply a function to a value whose type is the application of the
-- 'Compose' type constructor. This works under the 'Compose' newtype
-- wrapper.
onCompose :: (f (g a) -> h (k a)) -> (f :. g) a -> (h :. k) a
onCompose f = Compose . f . getCompose

type f :. g = Compose f g
infixr 9 :.

newtype Const (a :: *) (b :: k)
  = Const { getConst :: a }
    deriving ( Functor
             , Foldable
             , Traversable
             , Storable
             , Generic
             )

-- | A value with a phantom 'Symbol' label. It is not a
-- Haskell 'Functor', but it is used in many of the same places a
-- 'Functor' is used in vinyl.
--
-- Morally: newtype ElField (s, t) = Field t
-- But GHC doesn't allow that
newtype ElField (t :: (Symbol, Type)) = Field (Snd t)

deriving instance Eq t => Eq (ElField '(s,t))
deriving instance Ord t => Ord (ElField '(s,t))

instance KnownSymbol s => Generic (ElField '(s,a)) where
  type Rep (ElField '(s,a)) = C1 ('MetaCons s 'PrefixI 'False) (Rec0 a)
  from (Field x) = M1 (K1 x)
  to (M1 (K1 x)) = Field x

instance (Num t, KnownSymbol s) => Num (ElField '(s,t)) where
  Field x + Field y = Field (x+y)
  Field x * Field y = Field (x*y)
  abs (Field x) = Field (abs x)
  signum (Field x) = Field (signum x)
  fromInteger = Field . fromInteger
  negate (Field x) = Field (negate x)

instance Semigroup t => Semigroup (ElField '(s,t)) where
  Field x <> Field y = Field (x <> y)

instance (KnownSymbol s, Monoid t) => Monoid (ElField '(s,t)) where
  mempty = Field mempty
  mappend (Field x) (Field y) = Field (mappend x y)

instance (Real t, KnownSymbol s) => Real (ElField '(s,t)) where
  toRational (Field x) = toRational x

instance (Fractional t, KnownSymbol s) => Fractional (ElField '(s,t)) where
  fromRational = Field . fromRational
  Field x / Field y = Field (x / y)

instance (Floating t, KnownSymbol s) => Floating (ElField '(s,t)) where
  pi = Field pi
  exp (Field x) = Field (exp x)
  log (Field x) = Field (log x)
  sin (Field x) = Field (sin x)
  cos (Field x) = Field (cos x)
  asin (Field x) = Field (asin x)
  acos (Field x) = Field (acos x)
  atan (Field x) = Field (atan x)
  sinh (Field x) = Field (sinh x)
  cosh (Field x) = Field (cosh x)
  asinh (Field x) = Field (asinh x)
  acosh (Field x) = Field (acosh x)
  atanh (Field x) = Field (atanh x)

instance (RealFrac t, KnownSymbol s) => RealFrac (ElField '(s,t)) where
  properFraction (Field x) = fmap Field (properFraction x)

instance (Show t, KnownSymbol s) => Show (ElField '(s,t)) where
  show (Field x) = symbolVal (Proxy::Proxy s) ++" :-> "++show x

instance forall s t. (KnownSymbol s, Storable t)
    => Storable (ElField '(s,t)) where
  sizeOf _ = sizeOf (undefined::t)
  alignment _ = alignment (undefined::t)
  peek ptr = Field `fmap` peek (castPtr ptr)
  poke ptr (Field x) = poke (castPtr ptr) x
instance Show a => Show (Const a b) where
  show (Const x) = "(Const "++show x ++")"

instance Eq a => Eq (Const a b) where
  Const x == Const y = x == y

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance Show (f (g a)) => Show (Compose f g a) where
  show (Compose x) = show x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

instance Show a => Show (Identity a) where
  show (Identity x) = show x

instance Applicative Thunk where
  pure = Thunk
  (Thunk f) <*> (Thunk x) = Thunk (f x)

instance Monad Thunk where
  return = Thunk
  (Thunk x) >>= f = f x

instance Show a => Show (Thunk a) where
  show (Thunk x) = show x

instance (Functor f, Functor g) => Functor (Lift (,) f g) where
  fmap f (Lift (x, y)) = Lift (fmap f x, fmap f y)

instance (Functor f, Functor g) => Functor (Lift Either f g) where
  fmap f (Lift (Left x)) = Lift . Left . fmap f $ x
  fmap f (Lift (Right x)) = Lift . Right . fmap f $ x

instance (Applicative f, Applicative g) => Applicative (Lift (,) f g) where
  pure x = Lift (pure x, pure x)
  Lift (f, g) <*> Lift (x, y) = Lift (f <*> x, g <*> y)

-- $setup
-- >>> import Data.Vinyl.Core
-- >>> :set -XDataKinds
--

{- $example
    The data types in this module are used to build interpretation
    fuctions for a 'Rec'. To build a 'Rec' that is simply a heterogeneous
    list, use 'Identity':

>>> :{
let myRec1 :: Rec Identity '[Int,Bool,Char]
    myRec1 = Identity 4 :& Identity True :& Identity 'c' :& RNil
:}

    For a record in which the fields are optional, you could alternatively
    write:

>>> :{
let myRec2 :: Rec Maybe '[Int,Bool,Char]
    myRec2 = Just 4 :& Nothing :& Nothing :& RNil
:}

    And we can gather all of the effects with 'rtraverse':

>>> let r2 = rtraverse (fmap Identity) myRec2
>>> :t r2
r2 :: Maybe (Rec Identity '[Int, Bool, Char])
>>> r2
Nothing

    If the fields only exist once an environment is provided, you can
    build the record as follows:

>>> :{
let myRec3 :: Rec ((->) Int) '[Int,Bool,Char]
    myRec3 = (+5) :& (const True) :& (head . show) :& RNil
:}

    And again, we can collect these effects with "rtraverse":

>>> (rtraverse (fmap Identity) myRec3) 8
{13, True, '8'}

    If you want the composition of these two effects, you can use "Compose":

>>> import Data.Char (chr)
>>> :{
let safeDiv a b = if b == 0 then Nothing else Just (div a b)
    safeChr i = if i >= 32 && i <= 126 then Just (chr i) else Nothing
    myRec4 :: Rec (Compose ((->) Int) Maybe) '[Int,Char]
    myRec4 = (Compose $ safeDiv 42) :& (Compose safeChr) :& RNil
:}

-}

{- $ecosystem
    Of the five data types provided by this modules, three can
    be found in others places: "Identity", "Compose", and "Const".
    They are included with "vinyl" to help keep the dependency
    list small. The differences will be discussed here.

    The "Data.Functor.Identity" module was originally provided
    by "transformers". When GHC 7.10 was released, it was moved
    into "base-4.8". The "Identity" data type provided by that
    module is well recognized across the haskell ecosystem
    and has typeclass instances for lots of common typeclasses.
    The significant difference between it and the copy of
    it provided here is that this one has a different 'Show'
    instance. This is illustrated below:

>>> Identity "hello"
"hello"

    But, when using "Identity" from "base":

>>> import qualified Data.Functor.Identity as Base
>>> Base.Identity "hello"
Identity "hello"

    This 'Show' instance makes records look nicer in GHCi.
    Feel free to use "Data.Functor.Identity" if you do not
    need the prettier output or if you need the many additional
    typeclass instances that are provided for the standard
    "Identity".

    The story with "Compose" and "Const" is much more simple.
    These also exist in "transformers", although "Const"
    is named "Constant" there. Prior to the release of
    "transformers-0.5", they were not polykinded, making
    them unusable for certain universes. However, in
    "transformers-0.5" and forward, they have been made
    polykinded. This means that they are just as usable with 'Rec'
    as the vinyl equivalents but with many more typeclass
    instances such as 'Ord' and 'Show'.
-}
