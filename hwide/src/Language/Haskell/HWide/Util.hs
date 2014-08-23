{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
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
 
data FSItem = 
    Dir  {fsiPath :: FilePath}
  | File {fsiPath :: FilePath}
  deriving (Show,Read,Eq,Typeable)

instance Ord FSItem where
  (Dir _)  <= (File _) = True
  (File _) <= (Dir _)  = False
  f1       <= f2       = can f1 <= can f2
    where can = map toLower . takeFileName . fsiPath


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
        

getStaticDir :: IO FilePath
getStaticDir = do
  d <- (</> "wwwroot") `liftM` Paths_hwide.getDataDir
  ex <- doesDirectoryExist d
  if ex 
    then return d
    else do
      cd <- getCurrentDirectory
      return $ cd </> "wwwroot"

getMode :: FilePath -> T.Text
getMode fp = case takeExtension fp of
  ".hs"  -> "haskell"
  ".lhs" -> "haskell"
  _      -> "text"

getFileContents :: FilePath -> IO T.Text
getFileContents fp = do
  bs <- B.readFile fp
  return $ decodeUtf8 bs

setFileContents :: FilePath -> T.Text -> IO()
setFileContents fp cnts = 
  B.writeFile fp $ encodeUtf8 cnts
