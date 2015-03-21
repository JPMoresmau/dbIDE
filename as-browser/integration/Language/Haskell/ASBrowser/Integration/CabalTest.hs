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


cabalTests :: TestTree
cabalTests = testGroup "Cabal Tests" 
  [  testCase "updateFromCabal" $ do
      withTestAcid $ \acid -> do
        updateFromCabal acid
        let pkgAcid=PackageKey "acid-state" "0.12.3" Packaged
        mpkg1' <- query acid $ GetPackage pkgAcid
        isJust mpkg1' @? "acid-state not found"
  ]