{-# LANGUAGE TypeFamilies,TemplateHaskell, DeriveDataTypeable #-}
module Language.Haskell.ASBrowser.Database where

import Data.Acid

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Operations.Modules
import Language.Haskell.ASBrowser.Operations.Packages

$(makeAcidic ''Database
  [ 'writePackage
  , 'writePackages
  , 'deletePackage
  , 'getPackage
  , 'findPackages
  , 'writeModule
  , 'deleteModule
  , 'getModule
  , 'listModules
  , 'findModules
  ])