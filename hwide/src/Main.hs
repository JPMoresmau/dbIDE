{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,NamedFieldPuns #-}
-- | Main entry point
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Language.Haskell.HWide.Files
import Language.Haskell.HWide.UI.FileBrowser
import Language.Haskell.HWide.UI.FileList
import Language.Haskell.HWide.UI.PopupPane
import Language.Haskell.HWide.UI.UIUtils
import Language.Haskell.HWide.Util
import System.FilePath ((</>))
import qualified Data.Text as T

import qualified Data.Map as DM

-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig {tpStatic = Just static,tpCustomHTML=Just $ static </> "index.html"} setup

-- | Build UI
setup :: Window -> UI ()
setup w = do

  return w # set title "Haskell Web IDE"
  UI.addStyleSheet w "hwide.css"

  -- editor state event and behavior
  (evtEditorStateChange,fireEditorStateChange) <- liftIO newEvent
  bEditorState <- accumB mkEditorState evtEditorStateChange

  -- filebrowser component
  fb <- fileBrowser
  
  -- buttons
  closeFile <- UI.span #. "fileClose" # set UI.title__ "Close current file"
  setVisible closeFile False
  saveFile <- UI.span #. "fileSave" # set UI.title__ "Save current file"
  setVisible saveFile False
  
  -- current file list
  fileListData <- fileList (fbFileOpen fb) (UI.click closeFile)
  element (getElement fileListData) # set UI.title__ "Opened files"
  -- current file behavior
  let bCurrentFile = flbSelection fileListData
  
  -- force close event
  (eCloseFileBrowser,forceClose) <- liftIO newEvent
  -- popup pane for file browser
  elFileBrowserIcon <- popupPane ("fileBrowserIcon-Closed","fileBrowserIcon-Opened") ("Open File Browser","Close File Browser") fb eCloseFileBrowser
  
  -- hidden input notified of code mirror changes
  changeTick <- UI.input # set UI.type_ "hidden" # set UI.id_ "changeTick"

  let 
    -- create code mirror
    codeMirror :: T.Text -> JSFunction ()
    codeMirror= ffi "initCM(%1)"
    -- load code into code mirro
    loadCode :: T.Text -> T.Text -> JSFunction ()
    loadCode =  ffi "loadCM(%1,%2)"
    -- get current code from code mirror
    getCode :: JSFunction T.Text
    getCode =  ffi "getCMContents()"
    -- show file contents
    showContents fp mode contents=do
      liftIO $ forceClose ()
      runFunction $ loadCode mode contents
      es <- currentValue bEditorState
      case DM.lookup fp $ esFileInfos es of
        Just FileInfo{fiDirty} -> setVisible saveFile fiDirty
        Nothing -> do
          setVisible saveFile False
          liftIO $ fireEditorStateChange $ addFile fp
    showFile "" = do
      setVisible closeFile False
      showContents "" "haskell" ""
    showFile fp = do
      setVisible closeFile True
      s <- liftIO $ getFileContents fp
      showContents fp (getMode fp) s


  onChanges bCurrentFile showFile
  
  -- close file
  on UI.click closeFile (\_ -> do
    fp <- currentValue bCurrentFile
    liftIO $ fireEditorStateChange $ removeFile fp
    )
  
  -- save file
  on UI.click saveFile (\_ -> do
    cnts <- callFunction getCode
    fp <- currentValue bCurrentFile
    liftIO $ do
      setFileContents fp cnts
      liftIO $ fireEditorStateChange $ adjustFile fp setClean
    setVisible saveFile False
    )
  
  let
    idEditor = "textArea"
  -- text area anchoring the code mirror editor
  elText <- UI.textarea #. "codeEditor" # set UI.id_ idEditor
  

  let
    mkLayout :: UI Element
    mkLayout =
      column
        [row [column[element elFileBrowserIcon,element fb], element $ getElement fileListData, element closeFile, element saveFile]
        ,element elText,element changeTick]

  -- init
  layout <- mkLayout
  getBody w # set children [layout]
  runFunction $ codeMirror (T.pack $ '#' : idEditor)

  -- on each code mirror change, we set the current file to dirty
  on UI.sendValue changeTick (\_-> do
    setVisible saveFile True
    fp <- currentValue bCurrentFile
    liftIO $ fireEditorStateChange $ adjustFile fp setDirty
    )
   
  return ()

