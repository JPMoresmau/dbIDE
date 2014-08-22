{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Language.Haskell.HWide.UI.FileBrowser
import Language.Haskell.HWide.UI.FileList
import Language.Haskell.HWide.UI.PopupPane
import Language.Haskell.HWide.Util
import System.FilePath ((</>))
import qualified Data.Text as T
import Control.Monad (void)

-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig {tpStatic = Just static,tpCustomHTML=Just $ static </> "index.html"} setup

setup :: Window -> UI ()
setup w = do
  -- active elements
  return w # set title "Haskell Web IDE"
  UI.addStyleSheet w "hwide.css"
--  elInput <- UI.input # set value "" # set UI.droppable True
--  debug "start"
--  on (domEvent "livechange") elInput $ \_ -> do
--        s <- get value elInput
--        debug s
--  on (domEvent "drop") elInput $ \(EventData dts) -> do
--        s <- get value elInput
--        debug $ "drop:" ++ show dts  
-- myCodeMirror.setSize(500, 300);

  fileListData <- fileList

  fb <- fileBrowser
  
  -- elFileBrowserIcon <- popupPane ("/static/img/folder_closed.png","/static/img/folder.png") fb
  elFileBrowserIcon <- popupPane ("fileBrowserIcon-Closed","fileBrowserIcon-Opened") fb

  closeFile <- UI.span #. "fileClose" # set style [("display","none")]

  let 
    codeMirror :: T.Text -> JSFunction ()
    codeMirror= ffi "initCM(%1)"
    loadCode :: T.Text -> T.Text -> JSFunction ()
    loadCode =  ffi "loadCM(%1,%2)"
    showFile "" = do
      runFunction $ loadCode "haskell" "" 
      void $ element closeFile # set style [("display","none")]
    showFile fp = do
      ppClose elFileBrowserIcon
      s <- liftIO $ getFileContents fp
      runFunction $ loadCode (getMode fp) s
      void $ element closeFile # set style [("display","block")]

  on fbFileOpen fb (flAdd fileListData)

  on eSelection fileListData showFile

  on UI.click closeFile (\_ -> flClose fileListData)
  
  let
    idEditor = "textArea"
  elText <- UI.textarea #. "codeEditor" # set UI.id_ idEditor
  

  let
    mkLayout :: UI Element
    mkLayout =
      column
        [row [column[element elFileBrowserIcon,element fb], element $ flList fileListData, element $ closeFile],element elText]

        
  layout <- mkLayout
  getBody w # set children [layout]
  runFunction $ codeMirror (T.pack $ '#' : idEditor)
    
  return ()

