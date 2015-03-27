{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Language.Haskell.ASBrowser.TypesTest where

import Language.Haskell.ASBrowser.Types

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit
import Distribution.Version
import Data.String
import Data.Aeson
import Data.Aeson.Parser
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Attoparsec.ByteString.Lazy as L
import Control.Monad

typesTests :: TestTree
typesTests = testGroup "Types Tests" 
  [ testCase "IsString tests" $ do
      fromString "0.12.3" @?= Version [0,12,3] []
      fromString "acid-state"  @?= PackageName "acid-state"
  , testGroup "JSON round trip" 
      [ QC.testProperty "PackageName" (roundTripJSON:: PackageName -> Bool)
      , QC.testProperty "PackageNameCI" (roundTripJSON:: PackageNameCI -> Bool)
      , QC.testProperty "Version" (roundTripJSON:: Version -> Bool)
      , QC.testProperty "VersionRange" (roundTripJSON:: VersionRange -> Bool)
      ]
  ]
  

roundTripJSON :: (FromJSON a, ToJSON a,Eq a) => a -> Bool
roundTripJSON i =
    -- decode requires a top level value and our newtypes are encoded as strings!
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Data.Aeson.Success v) -> v == i
      _                    -> False


instance Arbitrary PackageName where
  arbitrary = PackageName <$> arbitrary
  
instance Arbitrary PackageNameCI where
  arbitrary = PackageNameCI <$> arbitrary

instance Arbitrary Version where
  arbitrary = do
    n <- choose (1, 4) :: Gen Int
    Version <$> (vectorOf n $ suchThat arbitrary (>=0)) <*> pure [] -- no tags

instance Arbitrary VersionRange where
  arbitrary = do
    n <- choose (0, 10) :: Gen Int
    simplifyVersionRange <$> case n of
      0 -> return anyVersion
      1 -> return noVersion
      2 -> thisVersion <$> arbitrary
      3 -> notThisVersion <$> arbitrary
      4 -> laterVersion <$> arbitrary
      5 -> earlierVersion <$> arbitrary
      6 -> orLaterVersion <$> arbitrary
      7 -> orEarlierVersion <$> arbitrary
      8 -> unionVersionRanges <$> simpleVersionRange <*> simpleVersionRange
      9 -> intersectVersionRanges <$> simpleVersionRange <*> simpleVersionRange
      10 -> withinVersion <$> arbitrary

simpleVersionRange = do
  n <- choose (0, 6) :: Gen Int
  case n of
    0 -> thisVersion <$> arbitrary
    1 -> notThisVersion <$> arbitrary
    2 -> laterVersion <$> arbitrary
    3 -> earlierVersion <$> arbitrary
    4 -> orLaterVersion <$> arbitrary
    5 -> orEarlierVersion <$> arbitrary
    6 -> withinVersion <$> arbitrary
