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
      , QC.testProperty "ComponentName" (roundTripJSON:: ComponentName -> Bool)
      , QC.testProperty "ComponentType" (roundTripJSON:: ComponentType -> Bool)
      , QC.testProperty "ModuleName" (roundTripJSON:: ModuleName -> Bool)
      , QC.testProperty "DeclName" (roundTripJSON:: DeclName -> Bool)
      , QC.testProperty "DeclType" (roundTripJSON:: DeclType -> Bool)
      , QC.testProperty "Local" (roundTripJSON:: Local -> Bool)
      , QC.testProperty "Expose" (roundTripJSON:: Expose -> Bool)
      , QC.testProperty "PackageMetaData" (roundTripJSON:: PackageMetaData -> Bool)
      , QC.testProperty "Doc" (roundTripJSON:: Doc -> Bool)
      , QC.testProperty "PackageKey" (roundTripJSON:: PackageKey -> Bool)
      , QC.testProperty "PackageRef" (roundTripJSON:: PackageRef -> Bool)
      , QC.testProperty "ComponentKey" (roundTripJSON:: ComponentKey -> Bool)
      , QC.testProperty "Component" (roundTripJSON:: Component -> Bool)  
      , QC.testProperty "Package" (roundTripJSON:: Package -> Bool)          
      , QC.testProperty "ModuleKey" (roundTripJSON:: ModuleKey -> Bool)
      , QC.testProperty "ModuleInclusion" (roundTripJSON:: ModuleInclusion -> Bool)  
      , QC.testProperty "Module" (roundTripJSON:: Module -> Bool)          
      , QC.testProperty "FullPackage" (roundTripJSON:: FullPackage -> Bool)
            
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
  arbitrary = simplifyVersionRange <$> oneof 
    [ return anyVersion
    , return noVersion
    , thisVersion <$> arbitrary
    , notThisVersion <$> arbitrary
    , laterVersion <$> arbitrary
    , earlierVersion <$> arbitrary
    , orLaterVersion <$> arbitrary
    , orEarlierVersion <$> arbitrary
    , unionVersionRanges <$> simpleVersionRange <*> simpleVersionRange
    , intersectVersionRanges <$> simpleVersionRange <*> simpleVersionRange
    , withinVersion <$> arbitrary
    ]


simpleVersionRange :: Gen VersionRange
simpleVersionRange = simplifyVersionRange <$> elements
  [thisVersion, notThisVersion, laterVersion, earlierVersion,
   orLaterVersion, orEarlierVersion, withinVersion]
    `ap` arbitrary


instance Arbitrary ComponentName where
  arbitrary = ComponentName <$> arbitrary 

instance Arbitrary ComponentType where
  arbitrary = elements [minBound..maxBound]
  
instance Arbitrary ModuleName where
  arbitrary = ModuleName <$> arbitrary

instance Arbitrary DeclName where
  arbitrary = DeclName <$> arbitrary
  
instance Arbitrary DeclType where
  arbitrary = elements [minBound..maxBound] 


instance Arbitrary Local where
  arbitrary = elements [minBound..maxBound]
  

instance Arbitrary Expose where
  arbitrary = oneof [return Exposed,return Included,Main <$> arbitrary]
 

instance Arbitrary PackageMetaData where
  arbitrary = PackageMetaData <$> arbitrary
  

instance Arbitrary Doc where
  arbitrary = Doc <$> arbitrary <*> arbitrary

instance Arbitrary PackageKey where
  arbitrary = PackageKey <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PackageRef where
  arbitrary = PackageRef <$> arbitrary <*> arbitrary

instance Arbitrary ComponentKey where
  arbitrary = ComponentKey <$> arbitrary <*> arbitrary  

instance Arbitrary Component where
  arbitrary = Component <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Package where
  arbitrary = Package <$> arbitrary <*> arbitrary <*> arbitrary  
  
instance Arbitrary ModuleKey where
  arbitrary = ModuleKey <$> arbitrary <*> arbitrary  

instance Arbitrary ModuleInclusion where
  arbitrary = ModuleInclusion <$> arbitrary <*> arbitrary  

instance Arbitrary Module where
  arbitrary = do
    m <- choose (1, 4) :: Gen Int
    Module <$> arbitrary <*> arbitrary <*> (vectorOf m arbitrary)  

instance Arbitrary FullPackage where
  arbitrary = do
    m <- choose (1, 4) :: Gen Int
    n <- choose (1, 10) :: Gen Int
    FullPackage <$> arbitrary <*> (vectorOf m arbitrary) <*> (vectorOf n arbitrary) 
