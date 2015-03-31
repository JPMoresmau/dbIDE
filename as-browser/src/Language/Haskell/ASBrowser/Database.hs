{-# LANGUAGE TypeFamilies,TemplateHaskell, DeriveDataTypeable, RecordWildCards #-}
module Language.Haskell.ASBrowser.Database where

import Data.Acid

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Operations.Components
import Language.Haskell.ASBrowser.Operations.Modules
import Language.Haskell.ASBrowser.Operations.Packages
import Control.Monad (void)

writeFullPackage :: FullPackage -> Update Database FullPackage
writeFullPackage fpk@FullPackage{..} = do
  void $ writePackage fpPackage
  mapM_ writeComponent fpComponents
  mapM_ writeModule fpModules
  return fpk

$(makeAcidic ''Database
  [ 'writePackage
  , 'writePackages
  , 'deletePackage
  , 'getPackage
  , 'findPackages
  , 'listVersions
  , 'getLatest
  , 'listMatching
  , 'getLatestMatching
  , 'listByLocal
  , 'allPackages
  , 'writeComponent
  , 'deleteComponent
  , 'getComponent
  , 'listComponents
  , 'findComponentsUsing
  , 'writeModule
  , 'deleteModule
  , 'getModule
  , 'listModules
  , 'findModules
  , 'writeFullPackage
  ])
  

