{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.ModulesTest where

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types

import Language.Haskell.ASBrowser.Operations.ComponentsTest
import Language.Haskell.ASBrowser.Operations.PackagesTest
import Language.Haskell.ASBrowser.TestHarness

import Test.Tasty
import Test.Tasty.HUnit

import Data.Acid
import Data.Default
import Data.IxSet
import Data.List

moduleTests :: TestTree
moduleTests = testGroup "Module Tests" 
  [ testCase "Write/Get/Delete" $
      withTestAcid $ \acid -> do
        mod1 <- update acid $ WriteModule testMod1
        mod1 @?= testMod1
        mmod1 <- query acid $ GetModule testModKey1
        mmod1 @?= Just testMod1
        mmod2 <- query acid $ GetModule testModKey2
        mmod2 @?= Nothing
        mod2 <- update acid $ WriteModule testMod2
        mod2 @?= testMod2
        update acid $ DeleteModule testModKey1
        mmod1'' <- query acid $ GetModule testModKey1
        mmod1'' @?= Nothing
        mmod2'' <- query acid $ GetModule testModKey2
        mmod2'' @?= Just testMod2
  , testCase "List" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteModule testMod1
        _ <- update acid $ WriteModule testMod2
        mods1 <- query acid $ ListModules testPkgKey1 Nothing
        sort (toList mods1) @?= [testMod1,testMod2]
        mods2 <- query acid $ ListModules testPkgKey1 $ Just $ cName $ testCompKey1
        toList mods2 @?= [testMod1]
  , testCase "Find" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteModule testMod1
        _ <- update acid $ WriteModule testMod2
        _ <- update acid $ WriteModule testMod3
        mods1 <- query acid $ FindModules [ComponentKey testPkgKey1 $ cName $ testCompKey1] ""
        sort (toList mods1) @?= [testMod1,testMod3]
        mods2 <- query acid $ FindModules [ComponentKey testPkgKey1 $ cName $ testCompKey1] "Test.M"
        toList mods2 @?= [testMod1]
        mods3 <- query acid $ FindModules [] "Test.M"
        toList mods3 @?= [testMod1,testMod2]
  ]
  
testModKey1 :: ModuleKey
testModKey1 = ModuleKey "Test.Module1" testPkgKey1

testMod1 :: Module
testMod1 = Module testModKey1 def Exposed [cName $ testCompKey1]

testModKey2 :: ModuleKey
testModKey2 = ModuleKey "Test.Module2" testPkgKey1

testMod2 :: Module
testMod2 = Module testModKey2 def Exposed [cName $ testCompKey2]

testModKey3 :: ModuleKey
testModKey3 = ModuleKey "Test.Utils" testPkgKey1

testMod3 :: Module
testMod3 = Module testModKey3 def Exposed [cName $ testCompKey1]
