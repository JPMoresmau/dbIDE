{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HWide.UI.FileBrowser where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Directory 
import System.FilePath
import Data.List (sort)
import Data.IORef

import Language.Haskell.HWide.Util
import Data.Text (Text)
import Control.Monad (unless)

data FileBrowser = FileBrowser 
  { fbElement :: Element
  , fbFileOpen :: Event FilePath
  }

instance Widget FileBrowser where
  getElement = fbElement

fileBrowser :: UI FileBrowser
fileBrowser = do
  cd <- liftIO $ canonicalizePath =<< getCurrentDirectory
  ior <- liftIO $ newIORef cd
  
  (evt,h) <- liftIO newEvent
  
  elFileList <- UI.div #. "fileBrowser"

  createFolder <- UI.image 
    # set UI.src "/static/img/folder_new.png" 
    # set UI.title__ "Create a new folder"
  createFile <- UI.image 
    # set UI.src "/static/img/file_new.png"
    # set UI.title__ "Create a new file"
  creates <- UI.div #. "fileBrowserButtons" # set children [createFolder,createFile]
  
  let
    prompt :: Text -> JSFunction String
    prompt = ffi "prompt(%1,'')"
    fillFileList = do
      dir <- liftIO $ canonicalizePath =<< readIORef ior
      fs <- liftIO $ listFiles dir
      lis <- mapM fileElem $ sort fs 
      lis2 <- if dir /= cd
        then do
          up <- fileElem $ Dir $ dir </> ".."
          return $ up:lis
        else return lis
      element elFileList # set children (creates : concat lis2)
      return ()
           
    fileElem :: FSItem -> UI [Element]
    fileElem fs = do
      let s =   takeFileName $ fsiPath fs
      a <- UI.span  # set text s #. fileCls fs
      on UI.click a $ \_ ->
        case fs of
          Dir fp  -> liftIO (writeIORef ior fp) >> fillFileList
          File fp -> liftIO $ h fp
      br <- UI.br
      return [a,br]
  
  on UI.click createFolder (\_ -> do
    fldr <- callFunction $ prompt "Enter new folder name"
    unless (null fldr) $ do
      liftIO $ do
        dir <- canonicalizePath =<< readIORef ior
        createDirectoryIfMissing False $ dir </> fldr
      fillFileList
    return ()
    )
  on UI.click createFile (\_ -> do
    file <- callFunction $ prompt "Enter new file name"
    unless (null file) $ do
      liftIO $ do
        dir <- canonicalizePath =<< readIORef ior
        writeFile (dir </> file) ""
      fillFileList
    return ()
    )
  
  fillFileList  
  return $ FileBrowser elFileList evt
  
fileCls :: FSItem -> String
fileCls (Dir _) = "folder"
fileCls (File fp) = case takeExtension fp of
  ".hs"    -> "hsfile"
  ".lhs"   -> "hsfile"
  ".cabal" -> "cabalfile"
  _        -> "file"
