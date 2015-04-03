{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, PatternGuards #-}
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
import qualified Data.Map as DM
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
import qualified Distribution.ModuleName as Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Data.Default
import Distribution.Text (display)
import Language.Haskell.ASBrowser.Operations.Modules
import Distribution.Version
import Network.URI hiding (query)
import Data.Version


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
    idxFile (Just rep) = do
      idxf <- getIndexFile rep
      return $ Just (rep,idxf)
    processIdx Nothing = return ()
    processIdx (Just (rep,fp)) = do
      toAdd <- unTarGzFileParMap fp processEntry
      let lasts=DM.fromListWith max $ catMaybes toAdd
      --let toInsert=catMaybes allPkgs
      --update acid $ WritePackages toInsert
      _ <- unTarGzFileParMap fp $ processEntry2 rep lasts
      createCheckpoint acid
    processEntry fp _ = do
      let dirs = splitDirectories fp
      case dirs of
        [pkg,ver,_] -> checkCabal pkg ver
        _ -> return Nothing
    checkCabal pkg ver = do
      let pkgKey = PackageKey (fromString pkg) (fromString ver) Packaged
      mPkg <- query acid $ GetPackage pkgKey
      if isNothing mPkg
        then return $ Just (pkg,(fromString ver)::Version)
        else return Nothing
    processEntry2 rep dm fp cnts = do
      let dirs = splitDirectories fp
      case dirs of
        [pkg,ver,_] 
          | Just ver2 <- DM.lookup pkg dm 
            -> processVersion rep cnts (fromString ver == ver2)
        _ -> return ()
    processVersion rep cnts isLast = 
      case packageFromDescriptionBS cnts (remotePackageLocation rep) of
          Just pkg -> if isLast 
                          then -- do
                            -- when ("acid-state" == pkgName (pkgKey $ fpPackage pkg)) $ print pkg
                            void $ scheduleUpdate acid $ WriteFullPackage pkg
                          else void $ scheduleUpdate acid $ WritePackage (fpPackage pkg)
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

data PackageLocation = PackageLocation
  { plLocal      :: Local
  , plPackageURL :: (PackageKey -> URLs)
  , plModuleURL  :: (Expose -> ModuleName -> URLs)
  }

localPackageLocation :: PackageLocation
localPackageLocation = PackageLocation Local (const $ URLs Nothing Nothing) (\_->const $ URLs Nothing Nothing) 

remotePackageLocation :: CabalRepositories -> PackageLocation
remotePackageLocation CabalRepositories{..} = build $ parse
  where
    parse = parseURI $ T.unpack crRemoteRepoURL
    build Nothing = noop
    build (Just uri) = root (uriAuthority uri) uri
    root Nothing _ = noop
    root (Just _) uri = loc $ uri{uriQuery="", uriFragment=""}
    noop = PackageLocation Packaged (const $ URLs Nothing Nothing) (\_->const $ URLs Nothing Nothing) 
    loc uri = PackageLocation Packaged (locKey uri) (locMod) 
    locKey uri key =
      let
        pn = T.unpack $ unPkgName $ pkgName key
        v = showVersion $ pkgVersion key
        uri2 = uri{uriPath="/package/"++pn++"-"++v}
      in URLs Nothing (Just $ URL $ T.pack $ show uri2)
    locMod e mn =URLs (src e mn) (doc e mn) 
    src _ mn = Just $ URL $ T.concat ["docs/src/",toDocURL mn,".html"]
    doc Exposed mn = Just $ URL $ T.concat ["docs/",toDocURL mn,".html"]
    doc _ _ = Nothing
    

packageFromDescriptionBS :: ByteString -> PackageLocation -> Maybe FullPackage
packageFromDescriptionBS cnts loc = 
        case parsePackageDescription $ toString cnts of
          ParseOk _ gpd -> Just $ packageFromDescription (flattenPackageDescription gpd) loc
          _ -> Nothing

packageFromDescription :: PackageDescription -> PackageLocation -> FullPackage
packageFromDescription PackageDescription{..} loc =
  let Cabal.PackageName name = Cabal.pkgName package
      version = Cabal.pkgVersion package
      pkgKey = PackageKey (PackageName $ T.pack name) version (plLocal loc)
      doc = Doc (T.pack synopsis) (T.pack description)
      md = PackageMetaData (T.pack author)
      fromBi = componentFromBuildInfo loc pkgKey buildDepends
      (cps,mods) = unzip $ maybe [] ((:[]) . fromBi (const "") Typ.Library libBuildInfo libToModule) library
                    ++ case (plLocal loc) of
                         Local ->
                              map (fromBi exeName Typ.Executable buildInfo exeToModule) executables
                           ++ map (fromBi testName Test testBuildInfo testToModule) testSuites
                           ++ map (fromBi benchmarkName BenchMark benchmarkBuildInfo benchToModule) benchmarks
                         _ -> []
  in FullPackage (Package pkgKey doc md (plPackageURL loc pkgKey)) cps (mergeModules $ concat mods)
      
componentFromBuildInfo :: PackageLocation -> PackageKey -> [Cabal.Dependency] -> (e -> String) -> ComponentType -> (e -> BuildInfo) -> (PackageLocation -> e -> PackageKey -> ComponentName -> [Module]) -> e -> (Component,[Module])
componentFromBuildInfo loc pkgKey deps key typ fbi fmods e=let
  bi = fbi e
  cpnKey = ComponentKey pkgKey . fromString
  allDeps = map packageRefFromDependency $ targetBuildDepends bi ++ deps
  exts=map (T.pack . display) $ defaultExtensions bi
  k = cpnKey $ key e
  allMods=fmods loc e pkgKey (cName k) ++  map (toModule loc Included pkgKey (cName k)) (otherModules bi)
  in (Component k typ allDeps exts,allMods)

packageRefFromDependency :: Cabal.Dependency -> PackageRef
packageRefFromDependency (Cabal.Dependency (Cabal.PackageName name) range) = PackageRef (PackageName $ T.pack name) range

toModule ::  PackageLocation -> Expose -> PackageKey -> ComponentName -> Cabal.ModuleName -> Module
toModule loc exp pkgKey name mn = toNamedModule loc exp pkgKey name (toMN mn)
 where
  toMN = ModuleName . T.intercalate "." . map T.pack . Cabal.components

toNamedModule :: PackageLocation -> Expose -> PackageKey -> ComponentName -> ModuleName -> Module
toNamedModule loc exp pkgKey name mn = Module (ModuleKey mn pkgKey) def [ModuleInclusion name exp] (plModuleURL loc exp mn)


libToModule :: PackageLocation -> Library -> PackageKey -> ComponentName -> [Module]
libToModule loc lib pkg c = map (toModule loc Exposed pkg c) $ exposedModules lib

exeToModule :: PackageLocation -> Executable -> PackageKey -> ComponentName -> [Module]
exeToModule loc exe pkg c = [toNamedModule loc (Main $ modulePath exe) pkg c "Main"]

testToModule :: PackageLocation -> TestSuite -> PackageKey -> ComponentName -> [Module]
testToModule loc ts pkg c
  | (TestSuiteExeV10 _ fp) <- testInterface ts =[toNamedModule loc (Main fp) pkg c "Main"]
  | (TestSuiteLibV09 _ mn) <- testInterface ts =[toModule loc Exposed pkg c mn]
  | otherwise = []
  
benchToModule :: PackageLocation -> Benchmark -> PackageKey -> ComponentName -> [Module]
benchToModule loc  ts pkg c
  | (BenchmarkExeV10 _ fp) <- benchmarkInterface ts =[toNamedModule loc (Main fp) pkg c "Main"]
  | otherwise = []