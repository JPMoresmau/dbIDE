{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.CabalTest where

import Language.Haskell.ASBrowser.Integration.Cabal
import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.TestHarness

import Data.Acid
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import System.Directory

cabalTests :: TestTree
cabalTests = testGroup "Cabal Tests" 
  [ testCase "getConfigValue" $ do
      Just "hackage" @=? getConfigValue "repo" ["repo: hackage"]
      Just "hackage" @=? getConfigValue "repo" ["repo:  hackage"]
      Just "hackage" @=? getConfigValue "repo" ["test: titi","repo: hackage"]
      Nothing @=? getConfigValue "repo" ["repository: hackage"]
  , testCase "parseConfig" $ do
      Just (CabalRepositories "/home/jpmoresmau/.cabal/packages" "hackage.haskell.org" "http://hackage.haskell.org/packages/archive")
        @=? parseConfig ["remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive",
                         "remote-repo-cache: /home/jpmoresmau/.cabal/packages"]
      Nothing @=? parseConfig ["remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive"]
  , testCase "getCabalRepositories" $ do
      rep <- getCabalRepositories
      isJust rep @? "rep is nothing" 
  , testCase "getIndexFile" $ do
      rep <- getCabalRepositories
      f <- getIndexFile $ fromJust rep
      ex <- doesFileExist f
      ex @? "indexFile does not exist"
  , testCase "updateFromCabal" $ do
      withTestAcid $ \acid -> do
        updateFromCabal acid
        let pkgAcid=PackageKey "acid-state" "0.12.3" Packaged
        mpkg1' <- query acid $ GetPackage pkgAcid
        isJust mpkg1' @? "acid-state not found"
  ]