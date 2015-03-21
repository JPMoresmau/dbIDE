module Language.Haskell.ASBrowser.Integration.FilesTest where

import Language.Haskell.ASBrowser.Integration.Cabal
import Language.Haskell.ASBrowser.Integration.Files

import Data.Maybe
--import System.Directory

import Test.Tasty
import Test.Tasty.HUnit

filesTests :: TestTree
filesTests = testGroup "Files Tests" 
  [ testCase "unTarGzFileMap" $
      testFileMap unTarGzFileMap
  , testCase "unTarGzFileParMap" $
      testFileMap unTarGzFileParMap
  ]
  
testFileMap :: (FilePath -> (a -> t -> IO a) -> IO [a1]) -> IO ()
testFileMap fn = do
      rep <- getCabalRepositories
      idxFile <- getIndexFile $ fromJust rep
      let cnt fp _ = return fp
      c <- fn idxFile cnt
      length c > 100 @? "zero contents"