{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.TH
  ( makeUniverse
  , makeUniverse'
  , Semantics(..)
  , semantics
  ) where

import Language.Haskell.TH
import Data.Vinyl.TyFun

makeUniverse :: Name -> Q [Dec]
makeUniverse u = makeUniverse' u ("El" ++ nameBase u)

makeUniverse' :: Name -> String -> Q [Dec]
makeUniverse' u elName = do
  let elu = mkName elName
  u' <- conT u

  tvs <- do
    el <- newName "el"
    tyfun <- conT ''TyFun
    return [KindedTV el (AppT (AppT tyfun u') StarT)]

  let cons = [NormalC elu []]
  return [DataD [] elu tvs cons []]

class TyRep r where
  asType :: r -> TypeQ
instance TyRep Name where
  asType = conT
instance TyRep (Q Type) where
  asType = id

data Semantics = forall s t. (TyRep t, TyRep s) => t :~> s

semantics :: Name -> [Semantics] -> Q [Dec]
semantics elu sems = sequence (map inst sems)
  where
    inst :: Semantics -> Q Dec
    inst (u :~> t) = do
      elu' <- conT elu
      u' <- asType u
      t' <- asType t
      return $ TySynInstD ''App [elu',u'] t'
