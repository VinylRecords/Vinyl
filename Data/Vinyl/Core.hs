{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Core where

import Data.Vinyl.TyFun

import Control.Applicative
import Data.Singletons

-- | A record is parameterized by a universe @u@, list of rows @rs@, a large
-- elimination @el@, and a type constructor @f@ to be applied to the
-- interpretation @el r@ of each of those @r@.
data Rec :: (TyFun u * -> *) -> (* -> *) -> [u] -> * where
  RNil :: Rec el f '[]
  (:&) :: !(f (el $ r)) -> !(Rec el f rs) -> Rec el f (r ': rs)
infixr :&

-- | Shorthand for a record with a single field. Lifts the field's
-- value into the chosen functor automatically.
(=:) :: Applicative f => Sing k -> el $ k -> Rec el f '[ k ]
sing =: x = pure x :& RNil

-- | Shorthand for a record with a single field. This is useful for
-- @Applicative@ or @Monad@ic intialization of records as in the idiom:
--
-- > dist $ myField <-: someIO <+> yourField <-: otherIO
(<-:) :: Sing r -> f (el $ r) -> Rec el f '[r]
_ <-: x = x :& RNil
infixr 6 <-:

-- | Records constructed using the above combinators will often be polymorphic
-- in their interpreter @el@. To avoid providing a type annotation, one can
-- provide their interpreters with a singleton tag and pass that in.
withUniverse :: (forall x. el x) -> Rec el f rs -> Rec el f rs
withUniverse _ x = x
{-# INLINE withUniverse #-}
