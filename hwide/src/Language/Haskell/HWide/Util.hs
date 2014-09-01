{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Utility functions
module Language.Haskell.HWide.Util where

import System.Directory 
import System.FilePath 
import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Data.Char (toLower)
import Paths_hwide
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding 
import Data.Hashable (hash)
 
-- | A File System item, wrapping the path
data FSItem = 
    -- | A directory
    Dir 
      {fsiPath :: FilePath}
  | -- | A path
    File 
      {fsiPath :: FilePath}
  deriving (Show,Read,Eq,Typeable)

-- | Instance for sorting
instance Ord FSItem where
  (Dir _)  <= (File _) = True
  (File _) <= (Dir _)  = False
  f1       <= f2       = can f1 <= can f2
    where can = map toLower . takeFileName . fsiPath


-- | List all files inside a given directory
listFiles :: FilePath -> IO [FSItem]
listFiles cd = do
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
        

-- | Get directory where static resources are kept
getStaticDir :: IO FilePath
getStaticDir = do
  d <- (</> "wwwroot") `liftM` Paths_hwide.getDataDir
  ex <- doesDirectoryExist d
  if ex 
    then return d
    else do
      cd <- getCurrentDirectory
      return $ cd </> "wwwroot"


-- | Get the CodeMirror file for the given text
getMode :: FilePath -> T.Text
getMode fp = case takeExtension fp of
  ".hs"  -> "haskell"
  ".lhs" -> "haskell"
  _      -> "text"


-- | Get the file contents as a Text. Assumes UTF-8 encoding
getFileContents :: FilePath -> IO T.Text
getFileContents fp = do
  bs <- B.readFile fp
  return $ decodeUtf8 bs


-- | Set the file contents as a Text. Assumes UTF-8 encoding
setFileContents :: FilePath -> T.Text -> IO ()
setFileContents fp cnts = 
  B.writeFile fp $ encodeUtf8 cnts


-- | Get the directory where hwide is going to store its info
getHWideDir ::  IO FilePath
getHWideDir = do
  userDir <- getAppUserDataDirectory "hwide"
  createDirectoryIfMissing True userDir
  return userDir
  
-- | Get the workspace specific directory
-- This is inside the app directory to not pollute the user's workspace
getHWideWorkspaceDir :: FilePath -> IO FilePath
getHWideWorkspaceDir root = do
  userDir <- getHWideDir
  let rootName = hash root 
  let workDir = userDir </> makeValid (show rootName)
  createDirectoryIfMissing False workDir
  return workDir

