{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload (runApp, app) where

import Language.Haskell.Reload.FileBrowser

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import Web.Scotty as S
import System.Directory
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (fromStrict)

app' :: S.ScottyM ()
app' = do
  get "/" $ do
    text "hello"

  get (regex "^/files(.*)$") $ do
    path <- param "1"
    fss <- liftIO $ do
      root <- getCurrentDirectory
      listFiles root path
    json fss
  get (regex "^/file/(.*)$") $ do
    path <- param "1"
    cnt <- liftIO $ do
      root <- getCurrentDirectory
      B.readFile (root </> path)
    setHeader "Content-Type" $ fromStrict $ getMIMEText path
    raw cnt

app :: IO Application
app = scottyApp app'

runApp :: IO ()
runApp = scotty 8080 app'
