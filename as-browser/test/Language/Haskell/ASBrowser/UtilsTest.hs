{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.UtilsTest where

import Language.Haskell.ASBrowser.Utils


import Test.Tasty
import Test.Tasty.HUnit

utilsTests :: TestTree
utilsTests = testGroup "Utils Tests" 
  [ testCase "Prefix tests" $ do
      prefixInterval "" @?= ("","")
      prefixInterval "a" @?= ("a","b")
      prefixInterval "ay" @?= ("ay","az")
      prefixInterval "az" @?= ("az","a{")
  ]