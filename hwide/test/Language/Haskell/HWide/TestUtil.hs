-- | Utility functions for tests
module Language.Haskell.HWide.TestUtil where

import System.Directory

import Language.Haskell.HWide.Cabal
import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util
import Reactive.Threepenny (newEvent)
import Data.Default (def)

-- | Get the useful directories
getDirectories :: IO Directories
getDirectories = do
  cd <- canonicalizePath =<< getTemporaryDirectory
  workDir <- getHWideWorkspaceDir cd
  logsDir <- getLogsDir workDir
  let sandboxDir = getSandboxDir workDir
  return $ Directories cd workDir logsDir sandboxDir Nothing
  
-- | Initialize things in the IO Monad
getTestState :: IO (StaticState,EditorState)
getTestState = do
  dirs <- getDirectories
  initState <- mkEditorState $ dWorkDir dirs
    
  (evtLogRun,fireLogRun) <- newEvent

  runQueue <- startRunToLogQueue fireLogRun
  
  let ss = StaticState def dirs runQueue evtLogRun
  return (ss,initState)