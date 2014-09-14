{-# OPTIONS  -fno-warn-orphans   #-}

{-# LANGUAGE AutoDeriveTypeable  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Data.Typeable.Promoted.List where

import Data.Typeable

deriving instance Typeable '[]
deriving instance Typeable '(:)
