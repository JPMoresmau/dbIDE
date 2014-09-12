{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,NamedFieldPuns, ScopedTypeVariables #-}
-- | Main entry point
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Language.Haskell.HWide.Cabal
import Language.Haskell.HWide.Cache
import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Notes
import Language.Haskell.HWide.UI.FileBrowser
import Language.Haskell.HWide.UI.FileList
import Language.Haskell.HWide.UI.PopupPane
import Language.Haskell.HWide.UI.UIUtils
import Language.Haskell.HWide.Util
import System.Directory (canonicalizePath,getCurrentDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import qualified Data.Text as T

import qualified Data.Map as DM
import Reactive.Threepenny (onChange)
import Control.Monad (liftM, when)
import Data.Default (def)
import Data.Maybe (catMaybes, mapMaybe)

import Paths_hwide

-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig {tpStatic = Just static,tpCustomHTML=Just $ static </> "index.html"} setup

-- | Get the useful directories
getDirectories :: IO Directories
getDirectories = do
  cd <- canonicalizePath =<< getCurrentDirectory
  workDir <- getHWideWorkspaceDir cd
  logsDir <- getLogsDir workDir
  let sandboxDir = getSandboxDir workDir
  return $ Directories cd workDir logsDir sandboxDir


-- | Initialize things in the IO Monad
initIO :: IO (StaticState,EditorState)
initIO = do
  dirs <- getDirectories
  initState <- mkEditorState $ dWorkDir dirs
    
  (evtLogRun,fireLogRun) <- newEvent

  runQueue <- startRunToLogQueue fireLogRun
  
  let ss = StaticState (esPaths initState) dirs runQueue evtLogRun
  return (ss,initState)


-- | Build UI
setup :: Window -> UI ()
setup w = do
  (ss,initState) <- liftIO initIO
  let dirs = ssDirectories ss


  mSandBox <- initSandboxDir ss
  case mSandBox of
    Just s -> scheduleRun ss s
    _      -> return ()

  return w # set title "Haskell Web IDE"
  UI.addStyleSheet w "hwide.css"

  -- editor state event and behavior
  (evtEditorStateChange,fireEditorStateChange) <- liftIO newEvent
  (evtEditorStateCurrentChange,fireEditorStateCurrentChange) <- liftIO newEvent
  let evtEditorStateCurrentChange2 = fmap setCurrent evtEditorStateCurrentChange
  bEditorState <- accumB initState (unionWith (.) evtEditorStateChange evtEditorStateCurrentChange2)

  (evtNoteCountChange,fireNoteCountChange) <- liftIO newEvent
  bNoteCountChange <- accumB def evtNoteCountChange

  liftIO $ onChange bEditorState $ saveEditorState $ dWorkDir dirs

  (evtCacheChange,fireCacheChange) <- liftIO newEvent
  bCacheState <- accumB def evtCacheChange

  -- filebrowser component
  fb <- fileBrowser (dRootDir dirs) $ Just (dWorkDir dirs,"workspace")
  
  on fbFileOpen fb $ liftIO . fireEditorStateChange . addFile
  
  -- buttons
  closeFile <- UI.span #. "fileClose" # set UI.title__ "Close current file"
  setVisible closeFile False
  saveFile <- UI.span #. "fileSave" # set UI.title__ "Save current file"
  setVisible saveFile False
  
  fileListData <- fileList bEditorState fireEditorStateCurrentChange
  element (getElement fileListData) # set UI.title__ "Opened files"
  
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
      showContents fp (getMIME fp) s


  onEvent evtEditorStateCurrentChange showFile
  
  -- close file
  on UI.click closeFile (\_ -> do
    fp <- liftM esCurrent $ currentValue bEditorState
    liftIO $ fireEditorStateChange $ removeFile fp
    )

  -- save file
  on UI.click saveFile (\_ -> do
    cnts <- callFunction getCode
    fp <- liftM esCurrent $ currentValue bEditorState
    liftIO $ do
      setFileContents fp cnts
      fireEditorStateChange $ adjustFile fp setClean
    setVisible saveFile False
    cfi <- getCachedFileInfo fp bCacheState fireCacheChange
    nc <- needsConfigure cfi
    --let mli = if nc then getConfigureInput ss cfi else getBuildInput ss cfi False
    --traverse (scheduleRun ss) mli
    let inputs = catMaybes $ [getConfigureInput ss cfi | nc]
                  ++ [getBuildInput ss cfi False]
    mapM (scheduleRun ss) inputs
    )
  
  onEvent (ssRunEvent ss) (handleRunLog ss fireNoteCountChange)
  
  let idEditor = "textArea"
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
    es <- currentValue bEditorState
    let fp = esCurrent es
    when (not (null fp) && Just True /= fmap fiDirty (DM.lookup fp $ esFileInfos es)) $ do
       setVisible saveFile True
       liftIO $ fireEditorStateChange $ adjustFile fp setDirty
    )
   
  liftIO $ onChange bNoteCountChange $ \v -> do
    writeToOut $ nNoteCount v
   
  return ()

handleRunLog :: StaticState -> Handler (Notes -> Notes) -> (RunToLogInput,RunToLogResult) -> UI()
handleRunLog ss fireNoteCountChange (i,r) = case rtliType i of
  CabalConfigure cfi -> liftIO $ do
    err <- readFile $ rtlrErrFile r
    let msgs = parseCabalMessages ss cfi err
    case (cfiCabalFile cfi,cfiRootPath cfi) of
      (Just c,Just root)-> fireNoteCountChange (addNotes root msgs . removeNotes root [c])
      _     -> return ()
    return ()
  CabalBuild  cfi -> liftIO $ do
    err <- readFile $ rtlrErrFile r
    out <- readFile $ rtlrOutFile r
    let msgs = parseBuildMessages ss cfi err
    let fps=mapMaybe getBuiltPath (lines out)
    case cfiRootPath cfi of
      Just root -> fireNoteCountChange (addNotes root msgs . removeNotes root fps)
      _      -> return ()
    return ()
  _            -> return ()

  
-- | Get directory where static resources are kept
getStaticDir :: IO FilePath
getStaticDir = do
  d <- Paths_hwide.getDataFileName "wwwroot"
  ex <- doesDirectoryExist d
  if ex 
    then return d
    else do
      cd <- getCurrentDirectory
      return $ cd </> "wwwroot"