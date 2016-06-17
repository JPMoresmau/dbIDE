{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload (runApp, app) where

import Language.Haskell.Reload.FileBrowser

import           Data.Aeson (Value(..))
import           Network.Wai (Application)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (fromStrict)
import Data.List (isInfixOf)

app' :: ScottyM ()
app' = do
  middleware $ staticPolicy (addBase "web")
  middleware $ logStdoutDev
  get "/" $ file "web/index.html"
  get (regex "^/files(.*)$") $ do
    path <- param "1"
    let norm = case path of
                ('/':r)->r
                _ -> path
    checkPath norm $ do
      fss <- liftIO $ do
        root <- getCurrentDirectory
        listFiles root norm
      json fss
  put (regex "^/files/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      ex <- liftIO $ do
              root <- getCurrentDirectory
              let full = root </> path
              ex <- doesDirectoryExist full
              createDirectoryIfMissing True full
              return ex
      if ex
        then status noContent204
        else status created201
      json Null
  delete (regex "^/files/(.*)$") $ do
      path <- param "1"
      checkPath path $ do
        ex <- liftIO $ do
                root <- getCurrentDirectory
                let full = root </> path
                ex <- doesDirectoryExist full
                when ex $ removeDirectoryRecursive full
                return ex
        if ex
          then status ok200
          else status noContent204
        json Null
  get (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      cnt <- liftIO $ do
        root <- getCurrentDirectory
        B.readFile (root </> path)
      setHeader "Content-Type" $ fromStrict $ getMIMEText path
      raw cnt
  put (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      b <- body
      ex <- liftIO $ do
              root <- getCurrentDirectory
              let p = root </> path
              exd <- doesDirectoryExist p
              if exd
                then
                  return Nothing
                else do
                  ex <- doesFileExist p
                  when (not ex) $ do
                    createDirectoryIfMissing True $ takeDirectory p
                  B.writeFile p b
                  return $ Just ex
      case ex of
        Just True -> status ok200
        Just False -> status created201
        Nothing -> status forbidden403
      json Null
  delete (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      ex <- liftIO $ do
              root <- getCurrentDirectory
              let p = root </> path
              ex <- doesFileExist p
              when ex $ do
                removeFile p
              return ex
      if ex
        then status ok200
        else status noContent204
      json Null

checkPath :: FilePath -> ActionM () -> ActionM ()
checkPath path f = do
  if (".." `isInfixOf` path || isAbsolute path)
    then do
      status notFound404
      json Null
    else f

app :: IO Application
app = scottyApp app'

runApp :: Int -> IO ()
runApp port = scotty port app'
