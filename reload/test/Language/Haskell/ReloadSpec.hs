{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.ReloadSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson (Value(..), object, (.=))

import           Language.Haskell.Reload (app)

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}
