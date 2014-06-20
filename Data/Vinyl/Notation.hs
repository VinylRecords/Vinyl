module Data.Vinyl.Notation
  ( (:~:)
  , (<-:)
  , (<:)()
  , (<+>)
  , (<<*>>)
  , (<<$>>)
  , (=:)
  , (~=)
  , Rec((:&))
  , Semantics((:~>))
  ) where

import Data.Vinyl.Constraint
import Data.Vinyl.Core
import Data.Vinyl.Operators
import Data.Vinyl.TH
