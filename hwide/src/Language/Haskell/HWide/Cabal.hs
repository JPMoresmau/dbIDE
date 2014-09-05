-- | Cabal operations
module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath
import Control.Monad (void, unless)
import Control.Concurrent (forkIO)

import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util

-- | Get the directory for the Cabal sandbox, creating it and initializing it if needed
getSandboxDir 
  :: FilePath -- | Root directory
  -> FilePath
getSandboxDir root = root </> "sandbox"


-- | Create sandbox if it doesn't exist
initSandboxDir :: Directories -> Paths -> IO ()
initSandboxDir dirs paths = do
  let sand = dSandboxDir dirs
  ex <- doesDirectoryExist sand
  unless ex $ do
    let logDir = dLogsDir dirs
    createDirectory sand
    void $ forkIO $ void $ runToLog (pCabalPath paths) sand ("sandbox_init",logDir) ["sandbox","init"]