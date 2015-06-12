{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.SrcTest where

import Data.Monoid

import Language.Haskell.ASBrowser.Integration.Src
import Language.Haskell.ASBrowser.Types

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as LBS


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
                let decls = parseHaddock f
                    decodes = filter (\d-> "decode" == (dName d)) decls
                decodes @?= [aesonDeclDecode]
        ]
--    ]
  ]


aesonDeclDecode :: Decl
aesonDeclDecode = Decl "decode" DeclFunction ""
    (Doc
        "Efficiently deserialize a JSON value from a lazy ByteString.\n If this fails due to incomplete or invalid input, Nothing is\n returned."
        $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?><div class=\"doc\"><p>Efficiently deserialize a JSON value from a lazy <code><a href=\"/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy.html#t:ByteString\">ByteString</a></code>."
            <> "\n If this fails due to incomplete or invalid input, <code><a href=\"/package/base-4.7.0.1/docs/Data-Maybe.html#v:Nothing\">Nothing</a></code> is"
            <> "\n returned.</p><p>The input must consist solely of a JSON document, with no trailing"
            <> "\n data except for whitespace.</p><p>This function parses immediately, but defers conversion.  See"
            <> "\n <code><a href=\"Data-Aeson.html#v:json\">json</a></code> for details.</p></div>")
