module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath
import Control.Monad (void, unless)
import Control.Concurrent (forkIO)

import Language.Haskell.HWide.Util

-- |Get the directory for the Cabal sandbox, creating it and initializing it if needed
getSandboxDir 
  :: FilePath -- | Root directory
  -> FilePath -- | Log directory
  -> IO FilePath
getSandboxDir root logDir = do
  let subDir = root </> "sandbox"
  ex <- doesDirectoryExist subDir
  unless ex $ do
    createDirectory subDir
    void $ forkIO $ void $ runToLog "cabal" subDir ("sandbox_init",logDir) ["sandbox","init"]
  return subDir
  
