{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.PackagesTest where

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types

import Language.Haskell.ASBrowser.TestHarness

import Test.Tasty
import Test.Tasty.HUnit

import Data.Acid
import Data.Default
import Data.IxSet

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
  ]
  

  
testPkgKey1 :: PackageKey
testPkgKey1 = PackageKey "pkg1" "0.0.1" Packaged

testPkg1 :: Package
testPkg1 = Package testPkgKey1 def def

testPkgKey2 :: PackageKey
testPkgKey2 = PackageKey "mypkg" "0.0.1" Packaged

testPkg2 :: Package
testPkg2 = Package testPkgKey2 def def


