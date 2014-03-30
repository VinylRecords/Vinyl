{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Core where

import Data.Singletons
import Control.Applicative
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.Idiom.LazyIdentity

-- | A record is parameterized by a list of fields and a type constructor
-- to be applied to the interpretation @El r@ of each of those @r@.
data Rec :: [k] -> (* -> *) -> * where
  RNil :: Rec '[] f
  (:&) :: !(f (El r)) -> !(Rec rs f) -> Rec (r ': rs) f
infixr :&

-- | Vinyl uses a Tarski-style universe encoding for its fields. @El@ is the
-- interpretation of (data)kinds representing codes of types.
type family El (r :: k) :: *

-- | Sections of @f ~> g@ are natural transformations from @f@ to @g@.
newtype (f ~> g) x = NT { runNT :: f x -> g x }

-- | Shorthand for a record with a single field. Lifts the field's
-- value into the chosen functor automatically.
(=:) :: Applicative f => Sing r -> El r -> Rec '[r] f
_ =: b = pure b :& RNil

-- | Shorthand for a record with a single field of an 'Applicative'
-- type. This is useful for @Applicative@ or @Monad@ic intialization
-- of records as in the idiom:
--
-- > dist $ myField <-: someIO <+> yourField <-: otherIO
(<-:) :: Applicative f => Sing r -> f (El r) -> Rec '[r] f
_ <-: b = b :& RNil
infixr 6 <-:

type PlainRec rs = Rec rs Identity
type LazyPlainRec rs = Rec rs LazyIdentity

-- | Fixes a polymorphic record into the 'Identity' functor.
toPlainRec :: (forall f. Applicative f => Rec rs f) -> PlainRec rs
toPlainRec xs = xs

toLazyPlainRec :: (forall f. Applicative f => Rec rs f) -> LazyPlainRec rs
toLazyPlainRec xs = xs

