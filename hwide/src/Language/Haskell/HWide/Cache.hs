{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Language.Haskell.HWide.Cache where

import Data.Default
import Data.Typeable (Typeable)
import Graphics.UI.Threepenny.Core

import qualified Data.Map as DM
import System.FilePath (takeDirectory)

import Language.Haskell.HWide.Util

data CachedFileInfo= CachedFileInfo
  {
    cfiCabalFile :: Maybe FilePath
  , cfiRootPath  :: Maybe FilePath
  } deriving (Read,Show,Eq,Ord,Typeable)


data CachedData = CachedData 
  { cdFileInfos :: DM.Map FilePath CachedFileInfo
  } deriving (Read,Show,Eq,Ord,Typeable)
  
instance Default CachedData where
  def = CachedData DM.empty
  

setCachedFileInfo :: FilePath -> CachedFileInfo -> CachedData -> CachedData
setCachedFileInfo file cfi cd@CachedData{..} = cd{cdFileInfos = DM.insert file cfi cdFileInfos}

mkCachedFileInfo :: Maybe FilePath -> CachedFileInfo
mkCachedFileInfo mCabalFile = CachedFileInfo mCabalFile $ fmap takeDirectory mCabalFile

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
 
      