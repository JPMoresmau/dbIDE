{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.PackagesTest where

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Operations.Packages
import Language.Haskell.ASBrowser.Types

import Language.Haskell.ASBrowser.TestHarness

import Test.Tasty
import Test.Tasty.HUnit

import Data.Acid
import Data.Default
import Data.IxSet.Typed
import Data.List
import Distribution.Version

packageTests :: TestTree
packageTests = testGroup "Package Tests"
  [ testCase "Write/Get/Delete" $
      withTestAcid $ \acid -> do
        pkg1 <- update acid $ WritePackage testPkg1
        pkg1 @?= testPkg1
        mpkg1 <- query acid $ GetPackage testPkgKey1
        mpkg1 @?= Just testPkg1
        mpkg2 <- query acid $ GetPackage testPkgKey2
        mpkg2 @?= Nothing
        pkg2 <- update acid $ WritePackage testPkg2
        pkg2 @?= testPkg2
        mpkg1' <- query acid $ GetPackage testPkgKey1
        mpkg1' @?= Just testPkg1
        mpkg2' <- query acid $ GetPackage testPkgKey2
        mpkg2' @?= Just testPkg2
        update acid $ DeletePackage testPkgKey1
        mpkg1'' <- query acid $ GetPackage testPkgKey1
        mpkg1'' @?= Nothing
        mpkg2'' <- query acid $ GetPackage testPkgKey2
        mpkg2'' @?= Just testPkg2
  , testCase "Prefix" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WritePackage testPkg1
        _ <- update acid $ WritePackage testPkg2
        pkgs1 <- query acid $ FindPackages ""
        pkgs2 <- query acid $ FindPackages "pkg1"
        pkgs3 <- query acid $ FindPackages "p"
        pkgs2' <- query acid $ FindPackages "Pkg1"
        pkgs3' <- query acid $ FindPackages "P"
        size pkgs1 @?= 2
        toList pkgs2 @?= [testPkg1]
        toList pkgs3 @?= [testPkg1]
        toList pkgs2' @?= [testPkg1]
        toList pkgs3' @?= [testPkg1]
  , testCase "Versions" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WritePackage testPkg1
        mpkg1 <- query acid $ GetLatest $ pkgName $ pkgKey testPkg1
        mpkg1 @?= Just testPkg1
        ipkg1 <- query acid $ ListVersions $ pkgName $ pkgKey testPkg1
        toList ipkg1 @?= [testPkg1]
        let testPkgKey1_2 = PackageKey "pkg1" "0.0.2" Packaged
            testPkg1_2 = Package testPkgKey1_2 def def def def def
        _ <- update acid $ WritePackage testPkg1_2
        mpkg2 <- query acid $ GetLatest $ pkgName $ pkgKey testPkg1_2
        mpkg2 @?= Just testPkg1_2
        ipkg2 <- query acid $ ListVersions $ pkgName $ pkgKey testPkg1
        sort (toList ipkg2) @?= [testPkg1,testPkg1_2]
        mpkg3 <- query acid $ GetLatest "unknown"
        mpkg3 @?= Nothing
        ipkg3 <- query acid $ ListVersions "unknown"
        toList ipkg3 @?= []
        allPkgs <- query acid $ FindPackages "pkg1"
        toList allPkgs @?= [testPkg1,testPkg1_2]
        onlyLastVersions allPkgs @?= [testPkg1_2]
  , testCase "Version references" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WritePackage testPkg1
        mpkg1 <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) anyVersion
        mpkg1 @?= Just testPkg1
        ipkg1 <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) anyVersion
        toList ipkg1 @?= [testPkg1]
        mpkg1' <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) (thisVersion $ pkgVersion $ pkgKey testPkg1)
        mpkg1' @?= Just testPkg1
        ipkg1' <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) (thisVersion $ pkgVersion $ pkgKey testPkg1)
        toList ipkg1' @?= [testPkg1]
        mpkg1'' <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1)
        mpkg1'' @?= Nothing
        ipkg1'' <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1)
        toList ipkg1'' @?= []
        let testPkgKey1_2 = PackageKey "pkg1" "0.0.2" Packaged
            testPkg1_2 = Package testPkgKey1_2 def def def def def
        _ <- update acid $ WritePackage testPkg1_2
        mpkg2 <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) anyVersion
        mpkg2 @?= Just testPkg1_2
        ipkg2 <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) anyVersion
        sort (toList ipkg2) @?= [testPkg1,testPkg1_2]
        mpkg2' <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) (thisVersion $ pkgVersion $ pkgKey testPkg1)
        mpkg2' @?= Just testPkg1
        ipkg2' <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) (thisVersion $ pkgVersion $ pkgKey testPkg1)
        toList ipkg2' @?= [testPkg1]
        mpkg2'' <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1)
        mpkg2'' @?= Just testPkg1_2
        ipkg2'' <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1)
        toList ipkg2'' @?= [testPkg1_2]
        mpkg2''' <- query acid $ GetLatestMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1_2)
        mpkg2''' @?= Nothing
        ipkg2''' <- query acid $ ListMatching (pkgName $ pkgKey testPkg1) (laterVersion $ pkgVersion $ pkgKey testPkg1_2)
        toList ipkg2''' @?= []
  , testCase "By Local" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WritePackage testPkg1
        let pkgKeyLoc = PackageKey "mypkg" "0.0.1" Local
            pkgLoc = Package pkgKeyLoc def def def def def
        _ <- update acid $ WritePackage pkgLoc
        locs <- query acid $ ListByLocal Local
        toList locs @?= [pkgLoc]
        pks <- query acid $ ListByLocal Packaged
        toList pks @?= [testPkg1]
  ]



testPkgKey1 :: PackageKey
testPkgKey1 = PackageKey "pkg1" "0.0.1" Packaged

testPkg1 :: Package
testPkg1 = Package testPkgKey1 def def def def def

testPkgKey2 :: PackageKey
testPkgKey2 = PackageKey "mypkg" "0.0.1" Packaged

testPkg2 :: Package
testPkg2 = Package testPkgKey2 def def def def def


