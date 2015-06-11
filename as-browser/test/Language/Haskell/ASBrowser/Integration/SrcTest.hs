{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.SrcTest where

import Language.Haskell.ASBrowser.Integration.Src
import Language.Haskell.ASBrowser.Types

import Test.Tasty
import Test.Tasty.HUnit

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
--    ]
  ]
