module Language.Haskell.ASBrowser.Integration.FilesTest where

import Language.Haskell.ASBrowser.Integration.Cabal
import Language.Haskell.ASBrowser.Integration.Files

import Data.Maybe
--import System.Directory

import Test.Tasty
import Test.Tasty.HUnit

filesTests :: TestTree
filesTests = testGroup "Files Tests" 
  [ testCase "unTarFileHandle" $ do
      rep <- getCabalRepositories
      f <- getIndexFile $ fromJust rep
      let cnt fp _=return fp
      c <- unTarFileHandle f cnt
      length c > 100 @? "zero contents"
--  , testCase "unTarTGzipTemp" $ do
--      rep <- getCabalRepositories
--      f <- getIndexFile $ fromJust rep
--      let cnt = ((return . length) =<<) . getDirectoryContents
--      c <- unTarGzipTemp f cnt
--      c > 100 @? "zero contents"
  ]