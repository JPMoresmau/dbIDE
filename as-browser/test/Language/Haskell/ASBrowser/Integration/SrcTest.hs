{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.SrcTest where

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
                let decls = parseHaddock aesonModule f
                testExact aesonDeclDecode decls
                testExact aesonDeclValue decls
                testNoLongDoc aesonDeclFromJSON decls
                testExact aesonDeclString decls
                testExact aesonDeclParseJSON decls
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

aesonModule :: ModuleKey
aesonModule= ModuleKey "Data.Aeson" (PackageKey "aeson" "0.9.1" Packaged)

aesonDeclDecode :: Decl
aesonDeclDecode = Decl (DeclKey "decode" aesonModule) DeclFunction ""
    (Doc
        "Efficiently deserialize a JSON value from a lazy ByteString.\n If this fails due to incomplete or invalid input, Nothing is\n returned."
        $ "<div class=\"doc\"><p>Efficiently deserialize a JSON value from a lazy <code><a href=\"/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy.html#t:ByteString\">ByteString</a></code>."
            <> "\n If this fails due to incomplete or invalid input, <code><a href=\"/package/base-4.7.0.1/docs/Data-Maybe.html#v:Nothing\">Nothing</a></code> is"
            <> "\n returned.</p><p>The input must consist solely of a JSON document, with no trailing"
            <> "\n data except for whitespace.</p><p>This function parses immediately, but defers conversion.  See"
            <> "\n <code><a href=\"Data-Aeson.html#v:json\">json</a></code> for details.</p></div>")

aesonDeclValue :: Decl
aesonDeclValue = Decl (DeclKey "Value" aesonModule) DeclData ""
    (Doc "A JSON value represented as a Haskell value." "<div class=\"doc\"><p>A JSON value represented as a Haskell value.</p></div>")

aesonDeclString :: Decl
aesonDeclString = Decl (DeclKey "String" aesonModule) DeclConstructor ""
    (Doc "" "")

aesonDeclFromJSON :: Decl
aesonDeclFromJSON = Decl (DeclKey "FromJSON" aesonModule) DeclClass ""
    (Doc "A type that can be converted from JSON, with the possibility of\n failure."
    "")

aesonDeclParseJSON :: Decl
aesonDeclParseJSON = Decl (DeclKey "gParseJSON" aesonModule) DeclMethod ""
    (Doc "This method (applied to defaultOptions) is used as the\n default generic implementation of parseJSON."
        $ "<div class=\"doc\"><p>This method (applied to <code><a href=\"Data-Aeson-Types.html#v:defaultOptions\">defaultOptions</a></code>) is used as the\n"
            <>" default generic implementation of <code><a href=\"Data-Aeson.html#v:parseJSON\">parseJSON</a></code>.</p></div>")
