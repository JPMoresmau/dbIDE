{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.ASBrowser.Operations.Packages where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.IxSet
import Data.List

import qualified Data.Text as T

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Utils

writePackage :: Package -> Update Database Package
writePackage pkg = do
  db@Database{..} <- get
  put $ db{dPackages=updateIx (pkgKey pkg) pkg dPackages}
  return pkg

writePackages :: [Package] -> Update Database ()
writePackages pkgs = do
  db@Database{..} <- get
  put $ db{dPackages=foldl' (\ix pkg->updateIx (pkgKey pkg) pkg ix) dPackages pkgs}
  return ()

deletePackage :: PackageKey -> Update Database ()
deletePackage key = do
  db@Database{..} <- get
  put $ db{dPackages=deleteIx key dPackages}


getPackage :: PackageKey -> Query Database (Maybe Package)
getPackage key = do 
  Database{..} <- ask
  return $ getOne $ dPackages @= key

findPackages :: T.Text -> Query Database (IxSet Package)
findPackages prf = do
  Database{..} <- ask
  if T.null prf 
    then return dPackages
    else return $ dPackages @>=< (textTupleToPackageNameCI $ prefixInterval $ unPkgNameCI $ textToPackageNameCI prf)
 