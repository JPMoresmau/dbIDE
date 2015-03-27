{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.UtilsTest where

import Language.Haskell.ASBrowser.Utils


import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Data.Maybe

utilsTests :: TestTree
utilsTests = testGroup "Utils Tests" 
  [ testCase "Prefix tests" $ do
      prefixInterval "" @?= ("","")
      prefixInterval "a" @?= ("a","b")
      prefixInterval "ay" @?= ("ay","az")
      prefixInterval "az" @?= ("az","a{")
  , QC.testProperty "safeLast" safeLast_prop
  , QC.testProperty "safeHead" safeLast_prop
  ]


safeLast_prop :: [Int] -> Bool
safeLast_prop [] = isNothing $ safeLast [] 
safeLast_prop list = safeLast list == Just (last list)

safeHead_prop :: [Int] -> Bool
safeHead_prop [] = isNothing $ safeHead [] 
safeHead_prop list = safeHead list == Just (head list)