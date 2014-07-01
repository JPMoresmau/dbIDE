module Language.Haskell.HWide.FileBrowser where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Directory 
import System.FilePath
import Data.List (sort)
import Data.IORef

import Language.Haskell.HWide.Util

fileBrowser :: (FilePath -> UI()) -> UI Element
fileBrowser onFile = do
  cd <- liftIO $ canonicalizePath =<< getCurrentDirectory
  ior <- liftIO $ newIORef cd
  
  elFileList <- UI.div #. "fileBrowser"
  
  let
    fillFileList = do
      dir <- liftIO $ canonicalizePath =<< readIORef ior
      fs <- liftIO $ listFiles dir
      lis <- mapM fileElem $ sort fs 
      lis2 <- if dir /= cd
        then do
          up <- fileElem $ Dir $ dir </> ".."
          return $ up:lis
        else return lis
      element elFileList # set children (concat lis2)
      return ()
           
    fileElem :: FSItem -> UI [Element]
    fileElem fs = do
      let s =   takeFileName $ fsiPath fs
      a <- UI.span  # set text s #. fileCls fs
      on UI.click a $ \_ ->
        case fs of
          Dir fp  -> liftIO (writeIORef ior fp) >> fillFileList
          File fp -> onFile fp
      br <- UI.br
      return [a,br]
  
  fillFileList  
  return elFileList  
  
fileCls :: FSItem -> String
fileCls (Dir _) = "folder"
fileCls (File fp) = case takeExtension fp of
  ".hs"    -> "hsfile"
  ".lhs"   -> "hsfile"
  ".cabal" -> "cabalfile"
  _        -> "file"
