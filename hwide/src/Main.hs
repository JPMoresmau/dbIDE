{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,NamedFieldPuns #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Language.Haskell.HWide.UI.FileBrowser
import Language.Haskell.HWide.UI.FileList
import Language.Haskell.HWide.UI.PopupPane
import Language.Haskell.HWide.UI.UIUtils
import Language.Haskell.HWide.Util
import System.FilePath ((</>))
import qualified Data.Text as T
import Data.IORef

import qualified Data.Map as DM
import Data.Typeable (Typeable)

-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig {tpStatic = Just static,tpCustomHTML=Just $ static </> "index.html"} setup

setup :: Window -> UI ()
setup w = do

  return w # set title "Haskell Web IDE"
  UI.addStyleSheet w "hwide.css"

  iorCurrentFile <- liftIO $ newIORef ""
  iorFileInfo <- liftIO $ newIORef (DM.singleton "" (FileInfo "" False))

  fileListData <- fileList
  element (getElement fileListData) # set UI.title__ "Opened files"
  fb <- fileBrowser
  
  -- elFileBrowserIcon <- popupPane ("/static/img/folder_closed.png","/static/img/folder.png") fb
  elFileBrowserIcon <- popupPane ("fileBrowserIcon-Closed","fileBrowserIcon-Opened") ("Open File Browser","Close File Browser") fb

  closeFile <- UI.span #. "fileClose" # set UI.title__ "Close current file"
  setVisible closeFile False
  saveFile <- UI.span #. "fileSave" # set UI.title__ "Save current file"
  setVisible saveFile False
  
  changeTick <- UI.input # set UI.type_ "hidden" # set UI.id_ "changeTick"

  let 
    codeMirror :: T.Text -> JSFunction ()
    codeMirror= ffi "initCM(%1)"
    loadCode :: T.Text -> T.Text -> JSFunction ()
    loadCode =  ffi "loadCM(%1,%2)"
    getCode :: JSFunction T.Text
    getCode =  ffi "getCMContents()"
    showContents fp mode contents=do
      ppClose elFileBrowserIcon
      liftIO (writeIORef iorCurrentFile fp)
      runFunction $ loadCode mode contents
      m <- liftIO $ readIORef iorFileInfo
      case DM.lookup fp m of
        Just FileInfo{fiDirty} -> setVisible saveFile fiDirty
        Nothing -> do
          setVisible saveFile False
          liftIO $ atomicModifyIORef' iorFileInfo (\m2 -> (DM.insert fp (FileInfo fp False) m2,()))
    showFile "" = do
      setVisible closeFile False
      showContents "" "haskell" ""
    showFile fp = do
      setVisible closeFile True
      s <- liftIO $ getFileContents fp
      showContents fp (getMode fp) s
      
  on fbFileOpen fb (flAdd fileListData)

  on eSelection fileListData showFile

  on UI.click closeFile (\_ -> do
    liftIO $ do
      fp <- readIORef iorCurrentFile
      atomicModifyIORef' iorFileInfo (\m -> (DM.delete fp m,()))
    flClose fileListData)
  
  on UI.click saveFile (\_ -> do
    cnts <- callFunction getCode
    liftIO $ do
      fp <- readIORef iorCurrentFile
      setFileContents fp cnts
      atomicModifyIORef' iorFileInfo (\m -> (DM.adjust setClean fp m,()))
    setVisible saveFile False
    )
  
  let
    idEditor = "textArea"
  elText <- UI.textarea #. "codeEditor" # set UI.id_ idEditor
  

  let
    mkLayout :: UI Element
    mkLayout =
      column
        [row [column[element elFileBrowserIcon,element fb], element $ getElement fileListData, element closeFile, element saveFile]
        ,element elText,element changeTick]

        
  layout <- mkLayout
  getBody w # set children [layout]
  runFunction $ codeMirror (T.pack $ '#' : idEditor)

   
  on UI.sendValue changeTick (\_-> do
    setVisible saveFile True
    liftIO $ do
      fp <- readIORef iorCurrentFile
      atomicModifyIORef' iorFileInfo (\m -> (DM.adjust setDirty fp m,()))
    )
   
  return ()

setDirty fi = fi {fiDirty=True}

setClean fi = fi {fiDirty=False}

data FileInfo = FileInfo
  { fiFile  :: FilePath
  , fiDirty :: Bool
  } deriving (Show,Read,Eq,Ord,Typeable)
