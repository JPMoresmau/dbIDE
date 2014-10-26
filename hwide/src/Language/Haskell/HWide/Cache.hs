{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
-- | Information that is cached (kept in memory but not flushed to configuration file)
module Language.Haskell.HWide.Cache where

import Data.Default
import Data.Typeable (Typeable)
import Graphics.UI.Threepenny.Core

import Language.Haskell.Ghcid

import qualified Data.Text as T

import qualified Data.Map as DM
import System.FilePath (takeDirectory)

import Language.Haskell.HWide.Util
import Control.Monad (join)
import Data.Maybe (fromMaybe)


-- | The whole cached data
data CachedData = CachedData 
  { cdFileInfos    :: DM.Map FilePath CachedFileInfo -- ^ File information
  , cdProjectInfos :: DM.Map FilePath ProjectInfo -- ^ project info
  , cdSandboxPkg   :: Maybe FilePath -- ^ path to the sandboxed package database
  } deriving (Typeable)
  
-- | Default instance
instance Default CachedData where
  def = CachedData DM.empty DM.empty Nothing
  

-- | Set file info for a given file
setCachedFileInfo :: FilePath -> CachedFileInfo -> CachedData -> CachedData
setCachedFileInfo file cfi cd@CachedData{..} = cd{cdFileInfos = DM.insert file cfi cdFileInfos}

-- | Create a file info for a file
mkCachedFileInfo :: Maybe FilePath -> CachedFileInfo
mkCachedFileInfo mCabalFile = CachedFileInfo mCabalFile (fmap takeDirectory mCabalFile) Nothing


-- | Update cached contents for a file
setCachedContents :: FilePath -> T.Text -> CachedData -> CachedData
setCachedContents file cnts cd@CachedData{..} = cd{cdFileInfos =DM.adjust (\cfi->cfi{cfiContents=Just cnts}) file cdFileInfos}

-- | Get cached contents if any
getCachedContents :: FilePath -> CachedData -> Maybe T.Text
getCachedContents fp = join . fmap cfiContents . DM.lookup fp . cdFileInfos


-- | Get cache file info, generate it if not found
getCachedFileInfo :: FilePath -> Behavior CachedData -> Handler (CachedData -> CachedData) -> UI CachedFileInfo
getCachedFileInfo fp b fire = do
  curr <- currentValue b
  let mcfi = DM.lookup fp $ cdFileInfos curr
  case mcfi of
    Just cfi -> return cfi
    Nothing  -> liftIO $ do
        cf <- getCabalFile fp
        let cfi = mkCachedFileInfo cf
        fire $ setCachedFileInfo fp cfi
        return cfi
 
setSandboxInit :: FilePath -> CachedData -> CachedData
setSandboxInit rootDir cd@CachedData{..} = let
  mpi = DM.lookup rootDir cdProjectInfos
  mpi2 = case mpi of
    Nothing  -> ProjectInfo True Nothing
    Just pif -> pif {piHasSandboxInit=True}
  in cd{cdProjectInfos = DM.insert rootDir mpi2 cdProjectInfos}

hasSandboxInit :: CachedFileInfo -> CachedData -> Bool
hasSandboxInit (CachedFileInfo _ (Just rootDir) _) CachedData{..} = 
  fromMaybe False $ piHasSandboxInit <$> DM.lookup rootDir cdProjectInfos
hasSandboxInit _ _ = False
