{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.TypesTest where

import Language.Haskell.ASBrowser.Types

import Test.Tasty
import Test.Tasty.HUnit
import Distribution.Version
import Data.String


typesTests :: TestTree
typesTests = testGroup "Types Tests" 
  [ testCase "IsString tests" $ do
      fromString "0.12.3" @?= Version [0,12,3] []
      fromString "acid-state"  @?= PackageName "acid-state"
  ]