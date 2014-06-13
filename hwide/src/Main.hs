module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Language.Haskell.HWide.FileBrowser
import Language.Haskell.HWide.Util



-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig {tpStatic = Just static} setup

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
  elFileList <- fileBrowser (\_->return ())
  
  let
    mkLayout :: UI Element
    mkLayout = column
            [row [element elFileList]]
        
  layout <- mkLayout
  getBody w # set children [layout]
  return ()
  
  