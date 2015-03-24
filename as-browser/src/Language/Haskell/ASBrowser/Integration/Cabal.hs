{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module Language.Haskell.ASBrowser.Integration.Cabal where

import System.FilePath
import System.Directory

import Control.Applicative
import Data.Char (isSpace)
import Data.Data
import Data.Monoid 
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.List
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C

import Language.Haskell.ASBrowser.Integration.Files
import Data.Acid
import Data.Acid.Advanced

import Language.Haskell.ASBrowser.Database
import Language.Haskell.ASBrowser.Types as Typ
import Control.Monad

import qualified Distribution.Package as Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Data.Default
import Distribution.Text (display)

data CabalRepositories = CabalRepositories
  { crCachePath :: FilePath
  , crRemoteRepoName :: T.Text
  , crRemoteRepoURL  :: T.Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

getCabalRepositories :: IO (Maybe CabalRepositories) 
getCabalRepositories = do
  config <- getCabalConfig
  ex <- doesFileExist config
  if ex
    then do
      cnts <- T.readFile config
      let remoteLines = filter (T.isPrefixOf "remote-") $ T.lines cnts
      return $ parseConfig remoteLines
    else return Nothing

parseConfig :: [T.Text] -> Maybe CabalRepositories
parseConfig ls = do
  repo <- getConfigValue "remote-repo" ls
  cache <- getConfigValue "remote-repo-cache" ls
  let (name,url)=T.breakOn ":" repo
  return $ CabalRepositories (T.unpack cache) name (T.dropWhile isSpace $ T.tail url)

getConfigValue :: T.Text -> [T.Text] -> Maybe T.Text
getConfigValue prf = listToMaybe 
  . map (T.dropWhile isSpace . T.drop (T.length prf + 1)) 
  . filter (T.isPrefixOf $ prf <> ":")

getCabalConfig :: IO FilePath
getCabalConfig = do
  dir<-getAppUserDataDirectory "cabal"
  return $ dir </> "config"


getCabalIndexPath :: FilePath -> T.Text -> FilePath
getCabalIndexPath cache repo = cache </> T.unpack repo </> indexFileName

indexFileName :: FilePath
indexFileName = "00-index.tar.gz"

getIndexFile :: CabalRepositories -> IO FilePath
getIndexFile CabalRepositories{..} = do
  let cache = getCabalIndexPath crCachePath crRemoteRepoName
  ex <- doesFileExist cache
  if ex
    then return cache
    else do
      tmp <- getTemporaryDirectory
      createDirectoryIfMissing True tmp
      let tmpFile = tmp </> indexFileName
      request <- parseUrl $ T.unpack crRemoteRepoURL ++ "/" ++ indexFileName
      withManager $ \manager -> do
         response <- http request manager
         responseBody response C.$$+- sinkFile tmpFile
      return tmpFile


updateFromCabal ::  AcidState Database -> IO ()
updateFromCabal acid = do
  processIdx =<< idxFile =<< getCabalRepositories 
  where
    idxFile Nothing = return Nothing
    idxFile (Just rep) = Just <$> getIndexFile rep
    processIdx Nothing = return ()
    processIdx (Just fp) = do
      _ <- unTarGzFileParMap fp processEntry
      --let toInsert=catMaybes allPkgs
      --update acid $ WritePackages toInsert
      createCheckpoint acid
    processEntry fp cnts = do
      let dirs = splitDirectories fp
      case dirs of
        [pkg,ver,_] -> processCabal pkg ver cnts
        _ -> return ()
    processCabal pkg ver cnts = do
      let pkgKey = PackageKey (fromString pkg) (fromString ver) Packaged
      mPkg <- query acid $ GetPackage pkgKey
      when (isNothing mPkg) $ processVersion cnts
    processVersion cnts = 
      case packageFromDescriptionBS cnts Packaged of
          Just pkg -> void $ scheduleUpdate acid $ WriteFullPackage pkg
          _ -> return ()
--    processPackage folder d = do
--      vrss <- getSubDirs $ folder </> d
--      mapM (processVersion folder d) vrss
--    processVersion folder d vr = do
--      let pkgKey = PackageKey (fromString d) (fromString vr) Packaged
--      mPkg <- query acid $ GetPackage pkgKey
--      if isNothing mPkg
--        then insertVersion folder d vr
--        else return Nothing
    
--    insertVersion folder d vr = do
--      let cf= addExtension d "cabal" 
--      let fullF= folder </> d </> vr </> cf
--      ex <- doesFileExist fullF
--      if ex 
--        then do
--          gpd <- readPackageDescription silent fullF
--          let pd=flattenPackageDescription gpd
--          let pkg = packageFromDescription pd Packaged
--          print $ pkgKey pkg
--          return $ Just pkg
--        else return Nothing
--        void $ update acid $ WritePackage pkg
--          let mlb=library pd
--          case mlb of
--            Just lib -> do
--              let deps = targetBuildDepends $ libBuildInfo lib
--              let depText= map toText deps
--              return $ DM.insert (T.pack vr) depText m
--            Nothing ->return m
--        else return m
--    toText (Dependency (PackageName name) range)=(T.pack name,T.pack $ show range)

packageFromDescriptionBS :: ByteString -> Local -> Maybe FullPackage
packageFromDescriptionBS cnts loc = 
        case parsePackageDescription $ toString cnts of
          ParseOk _ gpd -> Just $ packageFromDescription (flattenPackageDescription gpd) loc
          _ -> Nothing

packageFromDescription :: PackageDescription -> Local -> FullPackage
packageFromDescription PackageDescription{..} loc =
  let Cabal.PackageName name = Cabal.pkgName package
      version = Cabal.pkgVersion package
      pkgKey = PackageKey (PackageName $ T.pack name) version loc
      doc = Doc (T.pack synopsis) (T.pack description)
      md = PackageMetaData (T.pack author)
      cpnKey = ComponentKey pkgKey . fromString
      cps = maybe [] ((:[]) . componentFromBuildInfo (const $ cpnKey "") Typ.Library libBuildInfo) library
            ++ case loc of
                 Local ->
                      map (componentFromBuildInfo (cpnKey . exeName) Typ.Executable buildInfo) executables
                   ++ map (componentFromBuildInfo (cpnKey . testName) Test testBuildInfo) testSuites
                   ++ map (componentFromBuildInfo (cpnKey . benchmarkName) BenchMark benchmarkBuildInfo) benchmarks
                 _ -> []
  in FullPackage (Package pkgKey doc md) cps []
      
componentFromBuildInfo :: (e -> ComponentKey) -> ComponentType -> (e -> BuildInfo) -> e -> Component
componentFromBuildInfo key typ bi e=let
  exts=map (T.pack . display) $ defaultExtensions (bi e)
  in Component (key e) typ [] exts
