module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath
import Control.Monad (when)
import System.Process (readProcessWithExitCode)

getSandboxDir :: FilePath -> IO FilePath
getSandboxDir root = do
  let subDir = root </> "sandbox"
  ex <- doesDirectoryExist subDir
  when (not ex) $ do
    createDirectory subDir
    runCabal subDir ["sandbox","init"]
  return subDir
  

runCabal :: FilePath -> [String] -> IO ()
runCabal dir args = do
  cd<-getCurrentDirectory
  setCurrentDirectory dir
  (ex,out,err)<-readProcessWithExitCode "cabal" args ""
  setCurrentDirectory cd