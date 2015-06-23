{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.SrcTest where

import Data.Default
import Data.Monoid

import Language.Haskell.ASBrowser.Integration.Src
import Language.Haskell.ASBrowser.Types

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

srcTests :: TestTree
srcTests = testGroup "Src Tests"
  [ testCase "Pre HTML Content test" $ do
      getPreContentsLBS "" @?= ""
      getPreContentsLBS "<pre>line</pre>" @?= "line"
      getPreContentsLBS "<body><pre>line1body</pre></body>" @?= "line1body"
      getPreContentsLBS "<body><pre><span>line1span</span></pre></body>" @?= "line1span"
      getPreContentsLBS "<body><pre><span>line1</span>\n<span>line2</span></pre></body>" @?= "line1\nline2"
--  , testGroup "ParseModule" [
--      testCase "Data" $ do
--        let res = parseModule "-- | Simple module\nmodule Simple where\n-- | Simple data\ndata Test=Test Int\n" []
--        res @?= Right (Doc "Simple module\n" "",[Decl "Test" DeclData "" (Doc "Simple data\n" "")])
    , testGroup "ParseHaddock" [
            testCase "Aeson" $ do
                let fn= "data/Data.Aeson.html"
                f <- LBS.readFile fn
                let (aesonModule2,decls) = parseHaddock aesonModule f
                testExact aesonDeclDecode decls
                testExact aesonDeclValue decls
                testNoLongDoc aesonDeclFromJSON decls
                testExact aesonDeclString decls
                testExact aesonDeclParseJSON decls
                aesonModuleDoc @=? modDoc aesonModule2
        ]
--    ]
  ]

testExact :: Decl -> [Decl] -> IO ()
testExact d1 decls = do
    let ds = filter (\d-> (decName $ dKey d1) == (decName $ dKey d)) decls
    ds @?= [d1]

testNoLongDoc :: Decl -> [Decl] -> IO ()
testNoLongDoc d1 decls = do
    let ds = filter (\d-> (decName $ dKey d1) == (decName $ dKey d)) decls
    let noDoc = filter (T.null . dLong . dDoc) ds
    noDoc @?= []
    let ds2 = map (\d@Decl{..}->d{dDoc=dDoc{dLong=""}}) ds
    ds2 @?= [d1]

aesonModuleKey :: ModuleKey
aesonModuleKey = ModuleKey "Data.Aeson" (PackageKey "aeson" "0.9.1" Packaged)

aesonModule :: Module
aesonModule = Module aesonModuleKey def [] (URLs (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/src/Data-Aeson.html") (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html"))

aesonDeclDecode :: Decl
aesonDeclDecode = Decl (DeclKey "decode" aesonModuleKey) DeclFunction ""
    (Doc
        "Efficiently deserialize a JSON value from a lazy ByteString.\n If this fails due to incomplete or invalid input, Nothing is\n returned."
        $ "<div class=\"doc\"><p>Efficiently deserialize a JSON value from a lazy <code><a href=\"/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy.html#t:ByteString\">ByteString</a></code>."
            <> "\n If this fails due to incomplete or invalid input, <code><a href=\"/package/base-4.7.0.1/docs/Data-Maybe.html#v:Nothing\">Nothing</a></code> is"
            <> "\n returned.</p><p>The input must consist solely of a JSON document, with no trailing"
            <> "\n data except for whitespace.</p><p>This function parses immediately, but defers conversion.  See"
            <> "\n <code><a href=\"Data-Aeson.html#v:json\">json</a></code> for details.</p></div>")
    (URLs (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/src/Data-Aeson.html#decode")
        (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html#v:decode"))

aesonDeclValue :: Decl
aesonDeclValue = Decl (DeclKey "Value" aesonModuleKey) DeclData ""
    (Doc "A JSON value represented as a Haskell value." "<div class=\"doc\"><p>A JSON value represented as a Haskell value.</p></div>")
    (URLs  (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/src/Data-Aeson-Types-Internal.html#Value")
        (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html#t:Value"))

aesonDeclString :: Decl
aesonDeclString = Decl (DeclKey "String" aesonModuleKey) DeclConstructor ""
    (Doc "" "")
    (URLs Nothing (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html#v:String"))

aesonDeclFromJSON :: Decl
aesonDeclFromJSON = Decl (DeclKey "FromJSON" aesonModuleKey) DeclClass ""
    (Doc "A type that can be converted from JSON, with the possibility of\n failure."
    "")
    (URLs (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/src/Data-Aeson-Types-Class.html#FromJSON")
        (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html#t:FromJSON") )


aesonDeclParseJSON :: Decl
aesonDeclParseJSON = Decl (DeclKey "gParseJSON" aesonModuleKey) DeclMethod ""
    (Doc "This method (applied to defaultOptions) is used as the\n default generic implementation of parseJSON."
        $ "<div class=\"doc\"><p>This method (applied to <code><a href=\"Data-Aeson-Types.html#v:defaultOptions\">defaultOptions</a></code>) is used as the\n"
            <>" default generic implementation of <code><a href=\"Data-Aeson.html#v:parseJSON\">parseJSON</a></code>.</p></div>")
    (URLs (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/src/Data-Aeson-Types-Class.html#gParseJSON")
    (Just "https://hackage.haskell.org/package/aeson-0.9.0.1/docs/Data-Aeson.html#v:gParseJSON"))

aesonModuleDoc :: Doc
aesonModuleDoc = Doc "Types and functions for working efficiently with JSON data."
    "<div class=\"doc\"><p>Types and functions for working efficiently with JSON data.</p><p>(A note on naming: in Greek mythology, Aeson was the father of Jason.)</p></div>"
