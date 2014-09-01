{-# LANGUAGE OverloadedStrings #-}
-- | The file browser widget
module Language.Haskell.HWide.UI.FileBrowser where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Directory 
import System.FilePath
import Data.List (sort)

import Language.Haskell.HWide.Util
import Data.Text (Text)
import Control.Monad (unless)

-- | The FileBrowser Widget
data FileBrowser = FileBrowser 
  { fbElement :: Element -- ^ The graphical element
  , fbFileOpen :: Event FilePath -- ^ The even fired when a file is selected for opening
  }

-- | Widget instance
instance Widget FileBrowser where
  getElement = fbElement

-- | Build a file browser
fileBrowser
  :: FilePath -- ^ Root directory 
  -> UI FileBrowser
fileBrowser cd = do

  -- current folder: event -> behavior
  (evtNewCurrentFolder,fireNewCurrentFolder) <- liftIO newEvent
  -- shoulw we expose that behavior?
  bCurrentFolder <- stepper cd evtNewCurrentFolder
  -- open file event
  (evtOpenFile,fireOpenFile) <- liftIO newEvent
  
  -- UI
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
    -- list all file system items in the current folder and display them, including ..
    fillFileList :: FilePath -> UI ()
    fillFileList dir = do
      fs <- liftIO $ listFiles dir
      lis <- mapM fileElem $ sort fs 
      lis2 <- if not $ equalFilePath dir cd
        then do
          -- up link
          let upResult = takeDirectory dir
          up <- specialDirElem upResult ".."
          if not $ equalFilePath upResult cd 
            then do
              -- root link
              r <- specialDirElem cd "/"
              return $ r:up:lis
            else return $ up:lis
        else return lis
      element elFileList # set children (creates : concat lis2)
      return ()
    
    -- build special directories
    specialDirElem :: FilePath -> String ->  UI [Element]
    specialDirElem fp name = do
      a <- UI.span  # set text name #. fileCls (Dir fp) # set UI.title__ cd
      on UI.click a $ \_ -> liftIO $ fireNewCurrentFolder fp
      br <- UI.br
      return [a,br]
      
    -- build a single element       
    fileElem :: FSItem -> UI [Element]
    fileElem fs = do
      let fp = fsiPath fs
      let s = takeFileName fp
      a <- UI.span  # set text s #. fileCls fs # set UI.title__ fp
      on UI.click a $ \_ -> liftIO $
        case fs of
          Dir d  -> fireNewCurrentFolder d
          File f-> fireOpenFile f
      br <- UI.br
      return [a,br]
  
  -- folder creation
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
  -- file creation
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
  -- refresh view on change
  onEvent evtNewCurrentFolder fillFileList
  return $ FileBrowser elFileList evtOpenFile
  

-- | Determine the class of a file system item UI element
fileCls :: FSItem -> String
fileCls (Dir _) = "folder"
fileCls (File fp) = case takeExtension fp of
  ".hs"    -> "hsfile"
  ".lhs"   -> "hsfile"
  ".cabal" -> "cabalfile"
  _        -> "file"
