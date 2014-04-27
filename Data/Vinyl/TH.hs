{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Vinyl.TH where

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

