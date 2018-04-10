{-# LANGUAGE CPP, FlexibleInstances, InstanceSigs,
             MultiParamTypeClasses, ScopedTypeVariables,
             TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Concise vinyl record field lens syntax. This module exports an
-- orphan instance to make working with labels a bit more powerful. It
-- will conflict with other libraries that provide special syntax for
-- labels (i.e. placing a label in function application position, as
-- in @#age 23@, or using a label as a lens).
--
-- Example:
-- @fieldRec (#x =: True, #y =: 'b') :: FieldRec '[ '("x", Bool), '("y", Char) ]@
-- @fieldRec (#x =: True, #y =: 'b') & #x %~ not@
module Data.Vinyl.Syntax where
import Data.Vinyl.Derived (HasField, ElField(..), (:::), rfield)
import Data.Vinyl.Lens (RecElemFCtx, rlens')
import GHC.OverloadedLabels (IsLabel(..))
-- import GHC.TypeLits (KnownSymbol)

-- | Concise record construction syntax. Example: @record (#name "Joe", #age 23)@.
-- instance forall s a b. (KnownSymbol s, b ~ ElField '(s,a))
--   => IsLabel s (a -> b) where
-- #if __GLASGOW_HASKELL__ < 802
--   fromLabel _ = Field @s @a
-- #else
--   fromLabel = Field @s @a
-- #endif

-- | Concise 'ElField' lenses. Example @myRec & #name %~ map
-- toUpper@.
--
-- Credit to Tikhon Jelvis who shared this technique on the
-- Haskell-Cafe mailing list on December 23, 2017.
instance forall s t t' ts ts' f record a' b'.
  (HasField record s ts ts' t t', Functor f, RecElemFCtx record ElField,
   a' ~ (t -> f t'), b' ~ (record ElField ts -> f (record ElField ts')))
  => IsLabel s (a' -> b') where
#if __GLASGOW_HASKELL__ < 802
  fromLabel _ = rlens' @(s ::: t) . rfield
#else
  fromLabel :: (t -> f t') -> (record ElField ts -> f (record ElField ts'))
  fromLabel = rlens' @(s ::: t) . rfield
#endif
