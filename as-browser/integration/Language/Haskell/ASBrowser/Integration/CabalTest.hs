{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.CabalTest where

import Control.Applicative ((<$>))

import Language.Haskell.ASBrowser.Integration.Cabal
import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.TestHarness
import Language.Haskell.ASBrowser.Operations.Packages

import Data.Acid
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.IxSet
import Control.Monad

cabalTests :: TestTree
cabalTests = testGroup "Cabal Tests" 
  [  testCase "updateFromCabal" $ 
      withTestAcid $ \acid -> do
        updateFromCabal acid
        let pkgAcid=PackageKey "acid-state" "0.12.3" Packaged
        mpkg1 <- query acid $ GetPackage pkgAcid
        isJust mpkg1 @? "acid-state 0.12.3 not found"
        let urls = pkgURLs $ fromJust mpkg1
        urls @?= URLs Nothing (Just $ URL "http://hackage.haskell.org/package/acid-state-0.12.3") 
        mpkg2 <- query acid $ GetLatest "acid-state"
        isJust mpkg2 @? "acid-state latest not found"
        cpnts <- query acid $ ListComponents $ pkgKey $ fromJust mpkg2
        assertEqual "not 1 components" 1 (size cpnts)
        cName (cKey $ head $ toList cpnts) @?= ""
        mods <- query acid $ ListModules (pkgKey $ fromJust mpkg2) (Just "")
        size mods > 0 @? "no modules for acid-state library"
        let dam = filter (("Data.Acid" ==) . modName . modKey) $ toList mods
        assertEqual "not 1 Data.Acid module" 1 (length dam)
        modComponents (head dam) @?= [ModuleInclusion "" Exposed]
        modURLs (head dam) @?= URLs (Just $ URL "docs/src/Data-Acid.html")  (Just $ URL "docs/Data-Acid.html") 
        allPkgs <- liftM onlyLastVersions $ query acid $ ListByLocal Packaged
        let l=length allPkgs
        l >= 7793 && l < 9000 @? "wrong number of packages: " ++ show l
  ]