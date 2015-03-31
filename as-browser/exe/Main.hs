{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Main where

import Control.Applicative

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe


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

     
appInit :: SnapletInit App App
appInit = makeSnaplet "as-browser" "ASBrowser Snap app" (Just dataDir) $ do
  dbdir <- liftIO dbDir
  ac <- nestSnaplet "asb" acid $ acidInit' dbdir def
  addRoutes 
    [ ("/json", with acid jsonH)
    ]
  return $ App ac

jsonH :: Handler App (Acid Database) ()
jsonH= method GET $
  writeJSON =<< onlyLastVersions <$> query AllPackages

writeJSON :: (ToJSON a) => a -> Handler b c ()
writeJSON obj = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode obj

main :: IO ()
main = do
  ldir <- logDir
  createDirectoryIfMissing True ldir
  let cfg = setAccessLog (ConfigFileLog (ldir </> "access.log"))
              $ setErrorLog (ConfigFileLog (ldir </> "error.log"))
                defaultConfig
  serveSnaplet cfg appInit
