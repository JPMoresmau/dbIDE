{-# LANGUAGE OverloadedStrings,TemplateHaskell,DataKinds,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
module Language.Haskell.ASBrowser.UtilsTest where

import Language.Haskell.ASBrowser.Utils


import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Data.Maybe

import Data.IxSet.Typed
import Data.Typeable
import Data.Data


utilsTests :: TestTree
utilsTests = testGroup "Utils Tests"
  [ testCase "prefixInterval" $ do
      prefixInterval "" @?= ("","")
      prefixInterval "a" @?= ("a","b")
      prefixInterval "ay" @?= ("ay","az")
      prefixInterval "az" @?= ("az","a{")
  , testCase "splitSearchText" $ do
      splitSearchText '-' "aeson" @?= ([],("aeson","aesoo"))
      splitSearchText '-' "ac-angle" @?= (["ac"],("angle","anglf"))
  , QC.testProperty "safeLast" safeLast_prop
  , QC.testProperty "safeHead" safeLast_prop
  , testCase "substringAfter" $ do
    substringAfter "/" "foobar" @?= "foobar"
    substringAfter "=>" "(Eq a) => Ord a" @?= " Ord a"
  , testCase "ixsetRange" $ do
       let entries  = insertList [Entry (Content "word1 word4")] (empty :: IxEntry)
       let r = getRange (CWord "word2") (CWord "word3") entries
       size r @?= 0
  ]


safeLast_prop :: [Int] -> Bool
safeLast_prop [] = isNothing $ safeLast []
safeLast_prop list = safeLast list == Just (last list)

safeHead_prop :: [Int] -> Bool
safeHead_prop [] = isNothing $ safeHead []
safeHead_prop list = safeHead list == Just (head list)


data Entry      = Entry Content
   deriving (Show, Eq, Ord, Data, Typeable)
newtype Content = Content String
   deriving (Show, Eq, Ord, Data, Typeable)
newtype CWord = CWord String
   deriving (Show, Eq, Ord)

getWords :: Entry -> [CWord]
getWords (Entry (Content s)) = map CWord $ words s

type EntryIxs = '[CWord]
instance Indexable EntryIxs Entry where
     indices = ixList
                 (ixFun getWords)

type IxEntry = IxSet EntryIxs Entry
