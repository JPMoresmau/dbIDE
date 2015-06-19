{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Main where

import Control.Exception hiding (Handler)

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe

import Data.Acid as A (closeAcidState)
import Data.ByteString (ByteString)

import Language.Haskell.AsBrowser
import Snap.Snaplet
import Snap.Snaplet.AcidState
import Control.Lens.TH
import Control.Lens
import Data.Default
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Parser
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

dataDir :: IO FilePath
dataDir = liftM (</> "resources") getDataDir

dbDir :: IO FilePath
dbDir = liftM (</> "db") getDataDir


logDir :: IO FilePath
logDir = liftM (</> "log") getDataDir

data App = App
  { _acid :: Snaplet (Acid Database)
  }

makeLenses ''App

instance HasAcid App Database where
     getAcidStore = view (acid . snapletValue)


appInit :: AcidState Database -> SnapletInit App App
appInit st = makeSnaplet "as-browser" "ASBrowser Snap app" (Just dataDir) $ do
  onUnload (A.closeAcidState st)
  ac <- nestSnaplet "asb" acid $ acidInitManual st
  addRoutes
    [ ("/json/packages", with acid packagesH)
    , ("/json/versions", with acid versionsH)
    , ("/json/modules", with acid modulesH)
    , ("/json/decls", with acid declsH)
    , ("/static/", serveDirectory "resources")
    ]
  return $ App ac

packagesH :: Handler App (Acid Database) ()
packagesH = method GET $
  writeJSON =<< onlyLastVersions <$> query AllPackages

versionsH :: Handler App (Acid Database) ()
versionsH = do
  mkey <- getJSONParam "name"
  method GET $
    writeJSON =<<
      maybe (return [])
        (\ key -> (sortBy (comparing (Down . pkgVersion . pkgKey)) . toList) <$> query (ListVersions key))
        mkey

modulesH :: Handler App (Acid Database) ()
modulesH = do
  mkey <- getJSONParam "key"
  mcomp <- getJSONParam "component"
  method GET $
    writeJSON =<<
      maybe (return [])
        (\ key -> do
            state <- getAcidState
            liftIO $ ensurePackageModules key state
            toList <$> query (ListModules key mcomp))
        mkey

declsH :: Handler App (Acid Database) ()
declsH = do
  mkey <- getJSONParam "key"
  method GET $
    writeJSON =<<
      maybe (return [])
        (\ key -> do
            state <- getAcidState
            liftIO $ ensurePackageDecls (modPackageKey key) state
            toList <$> query (ListDecls key))
        mkey

getJSONParam :: (FromJSON a, MonadSnap m) =>
                  ByteString -> m (Maybe a)
getJSONParam name = do
  mkey <- getParam name
  let mv = maybe Nothing (maybeResult . parse value) mkey
  return $ maybe Nothing (parseMaybe parseJSON) mv


writeJSON :: (ToJSON a) => a -> Handler b c ()
writeJSON obj = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode obj

main :: IO ()
main = do
  ldir <- logDir
  dbdir <- liftIO dbDir
  state <- catch (openLocalStateFrom dbdir def)
                 (\(_::SomeException)-> do
                    cnts <- getDirectoryContents dbdir
                    forM_ cnts removeFile
                    openLocalStateFrom dbdir def
                    )
  _ <- forkIO $ updateFromCabal state
  createDirectoryIfMissing True ldir
  let cfg = setAccessLog (ConfigFileLog (ldir </> "access.log"))
              $ setErrorLog (ConfigFileLog (ldir </> "error.log"))
                defaultConfig
  serveSnaplet cfg $ appInit state
