{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Operations.DeclsTest where

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Operations.Decls
import Language.Haskell.ASBrowser.Operations.ModulesTest
import Language.Haskell.ASBrowser.Operations.ComponentsTest
import Language.Haskell.ASBrowser.Operations.PackagesTest
import Language.Haskell.ASBrowser.TestHarness

import Test.Tasty
import Test.Tasty.HUnit

import Data.Acid
import Data.Default
import Data.IxSet.Typed
import Data.List


declTests :: TestTree
declTests = testGroup "Decl Tests"
  [ testCase "Write/Get/Delete" $
      withTestAcid $ \acid -> do
        d1 <- update acid $ WriteDecl decl1
        d1 @?= decl1
        md1 <- query acid $ GetDecl declKey1
        md1 @?= Just decl1
        md2 <- query acid $ GetDecl declKey2
        md2 @?= Nothing
        d2 <- update acid $ WriteDecl decl2
        d2 @?= decl2
        update acid $ DeleteDecl declKey1
        md1'' <- query acid $ GetDecl declKey1
        md1'' @?= Nothing
        md2'' <- query acid $ GetDecl declKey2
        md2'' @?= Just decl2
   , testCase "List" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteDecl decl1
        _ <- update acid $ WriteDecl decl2
        ds1 <- query acid $ ListDecls testModKey1
        sort (toList ds1) @?= [decl1,decl2]
  , testCase "Find" $
      withTestAcid $ \acid -> do
        _ <- update acid $ WriteDecl decl1
        _ <- update acid $ WriteDecl decl2
        _ <- update acid $ WriteDecl decl3
        mods1 <- query acid $ FindDecls [testModKey1,testModKey2] "decl1"
        sort (toList mods1) @?= [decl1,decl3]
        mods2 <- query acid $ FindDecls [testModKey1] "decl1"
        toList mods2 @?= [decl1]
        mods3 <- query acid $ FindDecls [] "decl1"
        toList mods3 @?= [decl1,decl3]
        mods4 <- query acid $ FindDecls [testModKey1] "decl"
        toList mods4 @?= [decl1,decl2]
        mods5 <- query acid $ FindDecls [testModKey1] ""
        toList mods5 @?= [decl1,decl2]
  ]


declKey1 :: DeclKey
declKey1 = DeclKey "decl1" testModKey1

decl1 :: Decl
decl1 = Decl declKey1 DeclFunction "" def def

declKey2 :: DeclKey
declKey2 = DeclKey "decl2" testModKey1

decl2 :: Decl
decl2 = Decl declKey2 DeclFunction "" def def

declKey3 :: DeclKey
declKey3 = DeclKey "decl1" testModKey2

decl3 :: Decl
decl3 = Decl declKey3 DeclFunction "" def def
