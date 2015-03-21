{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.ComponentsTest where

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types

import Language.Haskell.ASBrowser.Operations.PackagesTest
import Language.Haskell.ASBrowser.TestHarness

import Test.Tasty
import Test.Tasty.HUnit

import Data.Acid
import Data.Default
import Data.IxSet
import Data.List
import Distribution.Version

componentTests :: TestTree
componentTests = testGroup "Component Tests" 
  [ testCase "Write/Get/Delete" $
      withTestAcid $ \acid -> do
        mod1 <- update acid $ WriteComponent testComp1
        mod1 @?= testComp1
        mmod1 <- query acid $ GetComponent testCompKey1
        mmod1 @?= Just testComp1
        mmod2 <- query acid $ GetComponent testCompKey2
        mmod2 @?= Nothing
        mod2 <- update acid $ WriteComponent testComp2
        mod2 @?= testComp2
        update acid $ DeleteComponent testCompKey1
        mmod1'' <- query acid $ GetComponent testCompKey1
        mmod1'' @?= Nothing
        mmod2'' <- query acid $ GetComponent testCompKey2
        mmod2'' @?= Just testComp2
  , testCase "List" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteComponent testComp1
        _ <- update acid $ WriteComponent testComp2
        mods1 <- query acid $ ListComponents testPkgKey1
        sort (toList mods1) @?= [testComp1,testComp2]
  , testCase "FindComponentsUsing" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteComponent testComp1
        _ <- update acid $ WriteComponent testComp2
        mods1 <- query acid $ FindComponentsUsing "base"
        sort (toList mods1) @?= [testComp1,testComp2]
        mods2 <- query acid $ FindComponentsUsing "text"
        toList mods2 @?= [testComp1]
  ]
  
testCompKey1 :: ComponentKey
testCompKey1 = ComponentKey testPkgKey1 ""

testComp1 :: Component
testComp1 = Component testCompKey1 Library [PackageRef "base" anyVersion,PackageRef "text" anyVersion]

testCompKey2 :: ComponentKey
testCompKey2 = ComponentKey testPkgKey1 "pkg1-test"

testComp2 :: Component
testComp2 = Component testCompKey2 Test  [PackageRef "base" anyVersion]
