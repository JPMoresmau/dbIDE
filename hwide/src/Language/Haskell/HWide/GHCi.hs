module Language.Haskell.HWide.GHCi where

import Language.Haskell.Ghcid
import Reactive.Threepenny

import qualified Data.Map as DM

import Language.Haskell.HWide.Cache
import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util


sandboxReady :: StaticState -> (Behavior CachedData) -> Handler (CachedData -> CachedData) -> FilePath -> IO ()
sandboxReady ss currentCache fireCacheChange rootDir = do
  cc <- currentValue currentCache
  let sandboxPkg = cdSandboxPkg cc
  let mpi= DM.lookup rootDir $ cdProjectInfos cc
  case mpi of
    Just (ProjectInfo _ (Just (GHCiInfo g _))) -> stopGhci g
    _ -> return ()
  let args="ghci ":
            case sandboxPkg of
              Just pkg-> ["--package-db="++pkg]
              _       ->[] 
  writeToOut $ unwords args
  (g,ls) <- startGhci (unwords args) (Just rootDir)
  fireCacheChange $ setGHCiInfo rootDir $ GHCiInfo g $ DM.fromList $ map (\l->(loadFile l,l)) ls
  

reloadGHCi :: StaticState -> (Behavior CachedData) -> Handler (CachedData -> CachedData) -> FilePath -> IO ()
reloadGHCi ss currentCache fireCacheChange rootDir = do
  cc <- currentValue currentCache
  let mpi= DM.lookup rootDir $ cdProjectInfos cc
  case mpi of
    Just (ProjectInfo _ (Just (GHCiInfo g _))) -> do
      ls<-reload g
      fireCacheChange $ setGHCiInfo rootDir $ GHCiInfo g $ DM.fromList $ map (\l->(loadFile l,l)) ls
    _ -> return ()

