-- | Cabal operations
module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath


import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util
import Graphics.UI.Threepenny.Core (MonadIO,liftIO)

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
      return $ Just $ RunToLogInput (pCabalPath $ ssPaths ss) sand ("sandbox_init",logDir) ["sandbox","init"]
    
