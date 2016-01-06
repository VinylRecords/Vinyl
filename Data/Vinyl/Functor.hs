{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Vinyl.Functor 
  ( -- * Introduction
    -- $introduction
    -- * Data Types
    Identity(..)
  , Thunk(..)
  , Lift(..)
  , Compose(..)
  , (:.)
  , Const(..)
    -- * Discussion
    
    -- ** Example
    -- $example
    
    -- ** Ecosystem
    -- $ecosystem
  ) where

import Control.Applicative hiding (Const)
import Data.Foldable
import Data.Traversable
import Foreign.Storable

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
    deriving (Storable)

type f :. g = Compose f g
infixr 9 :.

newtype Const (a :: *) (b :: k)
  = Const { getConst :: a }
    deriving ( Functor
             , Foldable
             , Traversable
             , Storable
             )

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

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
