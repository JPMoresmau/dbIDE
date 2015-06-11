{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.ASBrowser.Operations.Components where

import Control.Arrow ((***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.IxSet.Typed

import qualified Data.Text as T

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Utils

writeComponent :: Component -> Update Database Component
writeComponent cmp = do
  db@Database{..} <- get
  put $ db{dComponents=updateIx (cKey cmp) cmp dComponents}
  return cmp

deleteComponent :: ComponentKey -> Update Database ()
deleteComponent key = do
  db@Database{..} <- get
  put $ db{dComponents=deleteIx key dComponents}

getComponent :: ComponentKey -> Query Database (Maybe Component)
getComponent key = do
  Database{..} <- ask
  return $ getOne $ dComponents @= key

listComponents :: PackageKey -> Query Database IxComponent
listComponents key = do
  Database{..} <- ask
  return $ dComponents @= key

findComponentsUsing :: PackageName -> Query Database IxComponent
findComponentsUsing key = do
  Database{..} <- ask
  return $ dComponents @= key
