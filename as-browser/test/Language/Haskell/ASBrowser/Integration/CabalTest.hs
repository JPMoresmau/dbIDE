{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.CabalTest where

import Language.Haskell.ASBrowser.Integration.Cabal
import Language.Haskell.ASBrowser.Types
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import System.Directory
import Data.ByteString.Lazy (ByteString)
import Data.String
import Distribution.Version
import Data.Default

cabalTests :: TestTree
cabalTests = testGroup "Cabal Tests" 
  [ testCase "getConfigValue" $ do
      Just "hackage" @=? getConfigValue "repo" ["repo: hackage"]
      Just "hackage" @=? getConfigValue "repo" ["repo:  hackage"]
      Just "hackage" @=? getConfigValue "repo" ["test: titi","repo: hackage"]
      Nothing @=? getConfigValue "repo" ["repository: hackage"]
  , testCase "parseConfig" $ do
      Just (CabalRepositories "/home/jpmoresmau/.cabal/packages" "hackage.haskell.org" "http://hackage.haskell.org/packages/archive")
        @=? parseConfig ["remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive",
                         "remote-repo-cache: /home/jpmoresmau/.cabal/packages"]
      Nothing @=? parseConfig ["remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive"]
  , testCase "getCabalRepositories" $ do
      rep <- getCabalRepositories
      isJust rep @? "rep is nothing" 
  , testCase "getIndexFile" $ do
      rep <- getCabalRepositories
      f <- getIndexFile $ fromJust rep
      ex <- doesFileExist f
      ex @? "indexFile does not exist"
  , testCase "FullPackageWithLibrary" $ do
      let mfp = packageFromDescriptionBS cabalFile1 Packaged
      isJust mfp @? "mfp is nothing"
      let Just fp = mfp
      let pkg= fpPackage fp
      pkgName (pkgKey pkg) @?= "Pkg1"
      pkgVersion (pkgKey pkg) @?= "0.1"
      pkgLocal (pkgKey pkg) @?= Packaged
      length (fpComponents fp) @?= 1
      let lib = head $ fpComponents fp
      cPackageKey (cKey lib) @?= pkgKey pkg
      cName (cKey lib) @?= ""
      cExtensions lib @?= ["CPP","OverloadedStrings"]
      cRefs lib @?= [PackageRef (PackageName "base") ">=4 && <5"]
      length (fpModules fp) @?= 2
      fpModules fp @?= [modA,modBC]
  ]
  
cabalFile1 :: ByteString
cabalFile1 = fromString $ unlines 
  [ "name: Pkg1"
  , "version: 0.1"
  , "cabal-version:  >= 1.8"
  , "build-type:     Simple"
  , ""
  , "library"
  , "  hs-source-dirs:  src"
  , "  exposed-modules: A"
  , "  other-modules:  B.C"
  , "  build-depends:  base >=4 && <5"
  , "  default-extensions: CPP, OverloadedStrings"
  ]
  
modA :: Module
modA=Module (ModuleKey "A" (PackageKey "Pkg1" "0.1" Packaged)) def
          [ModuleInclusion "" Exposed]

modBC :: Module
modBC=Module (ModuleKey "B.C" (PackageKey "Pkg1" "0.1" Packaged)) def
          [ModuleInclusion "" Included]
