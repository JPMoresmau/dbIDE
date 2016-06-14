{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload.FileBrowserSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai.Test
import           Data.Aeson hiding (json)

import           Language.Haskell.Reload (app)
import           System.Directory
import           System.FilePath
import           Data.List
import           Data.Text (Text,toLower)
import qualified Data.HashMap.Strict as HM
import           Data.Ord
import           Data.Maybe
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = with app $ do
  describe "GET /files" $ do
    it "responds with files and folders at root" $ do
      js <- liftIO $ do
          root <- getCurrentDirectory
          fs <- getDirectoryContents root
          let visible = filter (not . ("." `isPrefixOf`) . takeFileName) fs
          sortBy sortFs <$> mapM (fileToJson root) visible
      get "/files" `shouldRespondWith` (fromValue $ toJSON js)
      get "/files/" `shouldRespondWith` (fromValue $ toJSON js)
    it "get source root folder" $ do
      get "/files/src" `shouldRespondWith` [json|[{"type":"dir","path":"src/Language"}]|]
      get "/files/src/" `shouldRespondWith` [json|[{"type":"dir","path":"src/Language"}]|]
  describe "GET /file" $ do
    it "get root plain file" $ do
      l <- liftIO $ B.readFile "LICENSE"
      get "/file/LICENSE" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain"]}
      r <- simpleBody <$> get "/file/LICENSE"
      liftIO $ r `shouldBe` l

fileToJson :: FilePath -> FilePath -> IO Value
fileToJson root fs = do
    let full =root</> fs
    isf <- doesFileExist full
    return $ if isf
        then let
          m =  HM.lookupDefault "text/plain" (takeExtension fs) myMimes
          in object ["type" .= ("file"::Text), "path" .= fs, "mime" .= m]
        else object ["type" .= ("dir"::Text), "path" .= fs]

sortFs :: Value -> Value -> Ordering
sortFs (Object m1) (Object m2) =
  let (String t1) = fromJust $ HM.lookup "type" m1
      (String t2) = fromJust $ HM.lookup "type" m2
      tc = t1 `compare` t2
  in if tc == EQ
      then
        let (String p1) = fromJust $ HM.lookup "path" m1
            (String p2) = fromJust $ HM.lookup "path" m2
        in (toLower p1) `compare` (toLower p2)
      else tc

myMimes :: HM.HashMap String Text
myMimes = HM.insert ".yaml" "text/x-yaml" $
          HM.insert ".cabal" "text/x-cabal" $
          HM.insert ".lhs" "text/x-haskell" $
          HM.insert ".hs" "text/x-haskell" HM.empty
