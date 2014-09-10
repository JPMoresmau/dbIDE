-- | Cabal operations
module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath


import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util
import Graphics.UI.Threepenny.Core (MonadIO,liftIO)
import Language.Haskell.HWide.Cache

-- | Get the directory for the Cabal sandbox, creating it and initializing it if needed
getSandboxDir 
  :: FilePath -- | Root directory
  -> FilePath
getSandboxDir root = root </> "sandbox"


-- | Create sandbox if it doesn't exist
initSandboxDir :: MonadIO m => StaticState -> m (Maybe RunToLogInput)
initSandboxDir ss = liftIO $ do
  let dirs = ssDirectories ss
  let sand = dSandboxDir dirs
  ex <- doesDirectoryExist sand
  if ex 
    then return Nothing
    else do
      let logDir = dLogsDir dirs
      createDirectory sand
      return $ Just $ RunToLogInput CabalSandbox (pCabalPath $ ssPaths ss) sand ("sandbox_init",logDir) ["sandbox","init"]
    

-- | get the full path for the dist directory (where cabal will write its output)
getDistDir ::  FilePath -> FilePath
getDistDir d= d </> "dist"

-- | get the configuration file for cabal
getSetupConfigFile :: FilePath -> FilePath
getSetupConfigFile d = d </> "setup-config"

-- | do we need to configure
needsConfigure :: MonadIO m => CachedFileInfo -> m Bool
needsConfigure (CachedFileInfo (Just cbl) (Just rootDir)) = liftIO $ do
  let setupFile = getSetupConfigFile $ getDistDir rootDir
  setupFileExists <- doesFileExist setupFile
  if setupFileExists
    then isMoreRecent cbl setupFile
    else return True
needsConfigure _ = return False

-- | get the run input for configure
getConfigureInput ::  StaticState -> CachedFileInfo -> Maybe RunToLogInput
getConfigureInput ss (CachedFileInfo (Just cbl) (Just rootDir)) = 
  let dirs = ssDirectories ss
      logDir = dLogsDir dirs
  in Just $ RunToLogInput CabalConfigure (pCabalPath $ ssPaths ss) rootDir ("configure-" ++ dropExtension (takeFileName cbl),logDir) ["configure","--enable-tests", "--enable-benchmarks"]
getConfigureInput _ _ =Nothing

-- | get the run input for build
getBuildInput ::  StaticState -> CachedFileInfo -> Bool -> Maybe RunToLogInput
getBuildInput ss (CachedFileInfo (Just cbl) (Just rootDir)) linking= 
  let dirs = ssDirectories ss
      logDir = dLogsDir dirs
      opts   = (if linking 
                                then []
                                else ["--ghc-option=-c"])
  in Just $ RunToLogInput CabalBuild (pCabalPath $ ssPaths ss) rootDir ("build-" ++ dropExtension (takeFileName cbl),logDir) ("build": opts)
getBuildInput _ _ _ =Nothing


 
 