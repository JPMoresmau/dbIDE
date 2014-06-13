{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.HWide.Util where

import System.Directory 
import System.FilePath 
import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Data.Char (toLower)
import Paths_hwide
import Control.Monad (liftM)

data FSItem = 
    Dir  {fsiPath :: FilePath}
  | File {fsiPath :: FilePath}
  deriving (Show,Read,Eq,Typeable)

instance Ord FSItem where
  (Dir _)  <= (File _) = True
  (File _) <= (Dir _)  = False
  f1       <= f2       = (can f1) <= (can f2)
    where can = map toLower . takeFileName . fsiPath


fileList :: FilePath -> IO [FSItem]
fileList cd = do
  fs <- getDirectoryContents cd
  let visible = filter (not . ("." `isPrefixOf`) . takeFileName) fs
  mapM tofs visible
  where
    tofs fp = do
      let full = cd </> fp
      isFile <- doesFileExist full
      can <- canonicalizePath full
      return $ if isFile
        then File can
        else Dir can
        

getStaticDir :: IO FilePath
getStaticDir = do
  d <- (</> "wwwroot") `liftM` Paths_hwide.getDataDir
  ex <- doesDirectoryExist d
  if ex 
    then return d
    else do
      cd <- getCurrentDirectory
      return $ cd </> "wwwroot"