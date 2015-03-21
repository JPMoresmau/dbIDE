{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.Modules where

import Control.Arrow ((***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.IxSet

import qualified Data.Text as T

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Utils

writeModule :: Module -> Update Database Module
writeModule mo = do
  db@Database{..} <- get
  put $ db{dModules=updateIx (modKey mo) mo dModules}
  return mo

deleteModule :: ModuleKey -> Update Database ()
deleteModule key = do
  db@Database{..} <- get
  put $ db{dModules=deleteIx key dModules}
  
getModule :: ModuleKey -> Query Database (Maybe Module)
getModule key = do 
  Database{..} <- ask
  return $ getOne $ dModules @= key

listModules :: PackageKey -> Maybe ComponentName -> Query Database (IxSet Module)
listModules key mcomp = do
  Database{..} <- ask
  case mcomp of
    Nothing   -> return $ dModules @= key
    Just comp -> return $ dModules @= ComponentKey key comp

findModules :: [ComponentKey] -> T.Text -> Query Database (IxSet Module)
findModules keys prf = do
  Database{..} <- ask
  let ix1 = case keys of
              [] -> dModules
              _  -> dModules @+ keys
  let ix2 = case prf of
              "" -> dModules
              _  -> dModules @>=< join (***) ModuleName (prefixInterval prf)
  return $ ix1 &&& ix2
