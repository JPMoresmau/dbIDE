{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.ASBrowser.Operations.Packages where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.IxSet
import Data.List

import qualified Data.Text as T

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Utils

import Distribution.Version

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
    else return $ dPackages @>=< textTupleToPackageNameCI (prefixInterval $ unPkgNameCI $ textToPackageNameCI prf)

listVersions :: PackageName -> Query Database (IxSet Package)
listVersions nm = do 
  Database{..} <- ask
  return $ dPackages @= nm

getLatest :: PackageName -> Query Database (Maybe Package)
getLatest = (lastInSet (pkgVersion . pkgKey) <$>) . listVersions

listMatching :: PackageName -> VersionRange -> Query Database (IxSet Package)
listMatching nm vr = do
  ix <- listVersions nm
  return $ fromList $ filter (\p-> withinRange (pkgVersion$ pkgKey p) vr) $ toList ix

getLatestMatching :: PackageName -> VersionRange -> Query Database (Maybe Package)
getLatestMatching nm = (lastInSet (pkgVersion . pkgKey) <$>) . listMatching nm

listByLocal :: Local ->  Query Database (IxSet Package)
listByLocal loc = do
  Database{..} <- ask
  return $ dPackages @= loc

onlyLastVersions :: IxSet Package -> [Package]
onlyLastVersions = map (last . snd) . (groupAscBy::IxSet Package -> [(PackageName, [Package])])

allPackages :: Query Database  (IxSet Package)
allPackages =  do
  Database{..} <- ask
  return dPackages
