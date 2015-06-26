{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Main where

import Network.Wai.Middleware.Static
import Web.Scotty
import Network.Wai.Handler.Launch

import Data.Acid as A

import Language.Haskell.AsBrowser
import Data.Default
import Data.Aeson hiding (json)
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Parser hiding (json)
import Data.List
import Data.Ord

import Paths_as_browser
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Control.Concurrent
import Data.IxSet.Typed (toList)

import Data.Attoparsec.ByteString

import Debug.Trace

import Data.Text.Lazy (Text)



dataDir :: IO FilePath
dataDir = liftM (</> "resources") getDataDir

dbDir :: IO FilePath
dbDir = liftM (</> "db") getDataDir


logDir :: IO FilePath
logDir = liftM (</> "log") getDataDir


main :: IO ()
main = do
  --ldir <- logDir
  dbdir <- liftIO dbDir
  putStrLn $ "Database folder: " ++ dbdir
  state <- openLocalStateFrom dbdir def
  _ <- forkIO $ updateFromCabal state
  --createDirectoryIfMissing True ldir
  --putStrLn $ "Log folder: " ++ ldir
  --let cfg = setAccessLog (ConfigFileLog (ldir </> "access.log"))
  --            $ setErrorLog (ConfigFileLog (ldir </> "error.log"))
  --              defaultConfig
  app <- scottyApp $ asBrowserApp state
  runUrlPort 3000 "" app

asBrowserApp :: AcidState Database -> ScottyM ()
asBrowserApp state = do
     middleware $ staticPolicy (noDots >-> addBase "resources")
     get "/" $ file "resources/index.html"
     get "/json/packages" $ do
            result <- liftIO $ onlyLastVersions <$> query state AllPackages
            json result
     get "/json/versions" $ do
            mkey <- getJSONParam "name"
            result <- liftIO $ maybe (return [])
                (\ key -> (sortBy (comparing (Down . pkgVersion . pkgKey)) . toList) <$> query state (ListVersions key))
                mkey
            json result
     get "/json/modules" $ do
        mkey <- getJSONParam "key"
        mcomp <- getJSONParam "component"
        result <- liftIO $ maybe (return [])
            (\ key -> do
                ensurePackageModules key state
                toList <$> query state (ListModules key mcomp))
            mkey
        json result
     get "/json/decls" $ do
        mkey <- getJSONParam "key"
        result <- liftIO $ maybe (return [])
          (\ key -> do
              ensurePackageDecls (modPackageKey key) state
              toList <$> query state (ListDecls key))
          mkey
        json result


getJSONParam ::       FromJSON a =>
                      Text
                      -> ActionM (Maybe a)
getJSONParam name= do
  mkey <- (Just <$> param name) `rescue` (const $ return Nothing)
  let mv = maybe Nothing (maybeResult . parse value) mkey
  return $ maybe Nothing (parseMaybe parseJSON) mv


