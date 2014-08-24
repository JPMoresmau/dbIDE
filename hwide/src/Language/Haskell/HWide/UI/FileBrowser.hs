{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HWide.UI.FileBrowser where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Directory 
import System.FilePath
import Data.List (sort)

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

  (evtNewCurrentFolder,fireNewCurrentFolder) <- liftIO newEvent
  bCurrentFolder <- stepper cd evtNewCurrentFolder
  (evtOpenFile,fireOpenFile) <- liftIO newEvent
  
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
    fillFileList dir = do
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
      let s = takeFileName $ fsiPath fs
      fp <- if s == ".." then liftIO $ canonicalizePath $ fsiPath fs else return $ fsiPath fs
      a <- UI.span  # set text s #. fileCls fs # set UI.title__ fp
      on UI.click a $ \_ -> liftIO $
        case fs of
          Dir d  -> fireNewCurrentFolder d
          File f-> fireOpenFile f
      br <- UI.br
      return [a,br]
  
  on UI.click createFolder (\_ -> do
    fldr <- callFunction $ prompt "Enter new folder name"
    unless (null fldr) $ do
      dir <- currentValue bCurrentFolder
      let ndir = dir </> fldr
      liftIO $ do
        createDirectoryIfMissing False ndir 
        fireNewCurrentFolder ndir
    return ()
    )
  on UI.click createFile (\_ -> do
    file <- callFunction $ prompt "Enter new file name"
    unless (null file) $ do
      dir <- currentValue bCurrentFolder
      liftIO $ do
        writeFile (dir </> file) ""
        fireNewCurrentFolder dir
    return ()
    )
  
  fillFileList cd
  onEvent evtNewCurrentFolder fillFileList
  return $ FileBrowser elFileList evtOpenFile
  
fileCls :: FSItem -> String
fileCls (Dir _) = "folder"
fileCls (File fp) = case takeExtension fp of
  ".hs"    -> "hsfile"
  ".lhs"   -> "hsfile"
  ".cabal" -> "cabalfile"
  _        -> "file"
