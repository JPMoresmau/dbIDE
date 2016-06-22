{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Reload.Build where

import Language.Haskell.Reload.FileBrowser

import Data.Aeson
import Language.Haskell.Ghcid
import Control.Monad
import Control.Concurrent
import System.FilePath
import Data.IORef

data BuildState = BuildState
  { bsRoot :: FilePath
  , bsBuildResult :: MVar Value
  , bsGhci :: IORef Ghci
  }

startBuild :: FilePath -> MVar Value -> IO BuildState
startBuild root buildResult = do
  (ghci,load) <- startGhci "stack repl" (Just root) (\_ _ -> return ())
  tryPutMVar buildResult $ loadsToValue root load
  mghci <- newIORef ghci
  return $ BuildState root buildResult mghci

rebuild :: BuildState -> IO ()
rebuild (BuildState root buildResult mghci) = void $ forkIO $ do
  ghci <- readIORef mghci
  interrupt ghci
  load <- reload ghci
  ok1 <- tryPutMVar buildResult $ loadsToValue root load
  when (not ok1) $ do
    tryTakeMVar buildResult
    void $ tryPutMVar buildResult $ loadsToValue root load

loadsToValue :: FilePath -> [Load] -> Value
loadsToValue root = toJSON . map (loadToValue root)

loadToValue :: FilePath -> Load -> Value
loadToValue root (Loading modu file)=object ["module".=modu,"file".=(relative root file),"mime".=getMIMEText file]
loadToValue root (Message sev file (ln,col) msg)=object ["severity".=(show sev),"file".=(relative root file),"line".=ln,"column".=col,"message".=msg,"mime".=getMIMEText file]

relative :: FilePath -> FilePath -> FilePath
relative root fp
  | isRelative fp = fp
  | otherwise = makeRelative root fp
