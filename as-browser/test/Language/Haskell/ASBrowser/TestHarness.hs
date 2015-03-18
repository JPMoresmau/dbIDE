module Language.Haskell.ASBrowser.TestHarness where

import Data.Acid
import System.Directory
import System.FilePath
import Control.Monad
import Data.Default
import Control.Exception

import Data.Acid.Local

import Language.Haskell.ASBrowser.Database ()
import Language.Haskell.ASBrowser.Types

withTempAcid :: (AcidState Database -> IO b) -> IO b
withTempAcid f = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "as-browser-tests"
  ex <- doesDirectoryExist dir
  when ex $ do
    cnts <- getDirectoryContents dir
    mapM_ removeFile =<< filterM doesFileExist (map (dir </>) cnts)
  bracket (openLocalStateFrom dir (def::Database))
          createCheckpointAndClose
          f