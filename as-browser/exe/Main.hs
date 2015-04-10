{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Main where

import Control.Applicative

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe

import Data.Acid as A (openLocalStateFrom,closeAcidState)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)

import Language.Haskell.AsBrowser
import Snap.Snaplet
import Snap.Snaplet.AcidState
import Control.Lens.TH
import Control.Lens
import Data.Default
import Data.Aeson

import Paths_as_browser
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Control.Concurrent
import Data.IxSet (toList)

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
    , ("/json/modules", with acid modulesH)
    , ("/static/", serveDirectory "resources")
    ]
  return $ App ac

packagesH :: Handler App (Acid Database) ()
packagesH = method GET $
  writeJSON =<< onlyLastVersions <$> query AllPackages

modulesH :: Handler App (Acid Database) ()
modulesH = do
  mkey <- getJSONParam "key"
  method GET $
    writeJSON =<<
      maybe (return [])
        (\ key -> toList <$> query (ListModules key Nothing))
        mkey


getJSONParam :: (FromJSON a, MonadSnap m) =>
                  ByteString -> m (Maybe a)
getJSONParam name = do
  mkey <- getParam name
  return $ maybe Nothing (decode . fromStrict) mkey


writeJSON :: (ToJSON a) => a -> Handler b c ()
writeJSON obj = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode obj

main :: IO ()
main = do
  ldir <- logDir
  dbdir <- liftIO dbDir
  state <- openLocalStateFrom dbdir def
  _ <- forkIO $ updateFromCabal state
  createDirectoryIfMissing True ldir
  let cfg = setAccessLog (ConfigFileLog (ldir </> "access.log"))
              $ setErrorLog (ConfigFileLog (ldir </> "error.log"))
                defaultConfig
  serveSnaplet cfg $ appInit state
