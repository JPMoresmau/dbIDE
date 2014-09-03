module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath
import Control.Monad (void, unless)
import Control.Concurrent (forkIO)

import Language.Haskell.HWide.Util

getSandboxDir :: FilePath -> FilePath -> IO FilePath
getSandboxDir root logDir = do
  let subDir = root </> "sandbox"
  ex <- doesDirectoryExist subDir
  unless ex $ do
    createDirectory subDir
    void $ forkIO $ void $ runToLog "cabal" subDir ("sandbox_init",logDir) ["sandbox","init"]
  return subDir
  
