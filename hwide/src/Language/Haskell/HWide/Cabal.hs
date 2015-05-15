{-# LANGUAGE PatternGuards,FlexibleContexts #-}
-- | Cabal operations
module Language.Haskell.HWide.Cabal where

import System.Directory
import System.FilePath

--import Control.Applicative
import Language.Haskell.HWide.Config
import Language.Haskell.HWide.Util
import Graphics.UI.Threepenny.Core (MonadIO,liftIO)
import Language.Haskell.HWide.Notes

import Text.Regex.TDFA
import Data.List
import Data.Char (isSpace)

-- | Get the directory for the Cabal sandbox, creating it and initializing it if needed
getSandboxDir
  :: FilePath -- ^ Root directory
  -> FilePath
getSandboxDir root = root </> "sandbox"


-- | Get the path to the sandboxed package database
getSandboxPackageDB :: FilePath -> IO FilePath
getSandboxPackageDB sandboxDir = do
    dbs <- extractDB <$> readSConfig
    case dbs of
      (db:_) -> return db
      []     -> error "Cannot extract package db"
  where field       = "package-db:"
        len         = length field
        readSConfig = readFile $ sandboxDir </> "cabal.sandbox.config"
        extractDB   =  map (drop len) . filter (field `isPrefixOf`) . lines


-- | Create sandbox if it doesn't exist
initSandboxDir :: MonadIO m => StaticState -> m (Either RunToLogInput FilePath)
initSandboxDir ss = liftIO $ do
  let dirs = ssDirectories ss
  let sand = dSandboxDir dirs
  ex <- doesDirectoryExist sand
  if ex
    then Right <$> getSandboxPackageDB sand
    else do
      let logDir = dLogsDir dirs
      createDirectory sand
      return $ Left $ RunToLogInput CabalSandbox (pCabalPath $ ssPaths ss) sand ("sandbox_init",logDir) ["sandbox","init"]


-- | get the full path for the dist directory (where cabal will write its output)
getDistDir ::  FilePath -> FilePath
getDistDir d= d </> "dist"

-- | get the configuration file for cabal
getSetupConfigFile :: FilePath -> FilePath
getSetupConfigFile d = d </> "setup-config"

-- | do we need to init the sandbox for that project
needsSandboxInit :: MonadIO m => CachedFileInfo -> m Bool
needsSandboxInit (CachedFileInfo _ (Just rootDir) _) = liftIO $ do
  let sandboxFile = rootDir </> "cabal.sandbox.config"
  doesFileExist sandboxFile
needsSandboxInit _ = return False

-- | get the run input for sandbox init
getSandboxInitInput :: StaticState -> CachedFileInfo -> Maybe RunToLogInput
getSandboxInitInput ss (CachedFileInfo (Just cbl) (Just rootDir) _) =
  let dirs = ssDirectories ss
      logDir = dLogsDir dirs
      sand = dSandboxDir dirs
  in Just $ RunToLogInput (CabalSandboxProject rootDir) (pCabalPath $ ssPaths ss) rootDir ("sandbox-init-" ++ dropExtension (takeFileName cbl),logDir) ["sandbox","init", "--sandbox",sand]
getSandboxInitInput _ _ =Nothing

-- | do we need to configure
needsConfigure :: MonadIO m => CachedFileInfo -> m Bool
needsConfigure (CachedFileInfo (Just cbl) (Just rootDir) _) = liftIO $ do
  let setupFile = getSetupConfigFile $ getDistDir rootDir
  setupFileExists <- doesFileExist setupFile
  if setupFileExists
    then isMoreRecent cbl setupFile
    else return True
needsConfigure _ = return False

-- | get the run input for configure
getConfigureInput ::  StaticState -> CachedFileInfo -> Maybe RunToLogInput
getConfigureInput ss cfi@(CachedFileInfo (Just cbl) (Just rootDir) _) =
  let dirs = ssDirectories ss
      logDir = dLogsDir dirs
  in Just $ RunToLogInput (CabalConfigure cfi) (pCabalPath $ ssPaths ss) rootDir ("configure-" ++ dropExtension (takeFileName cbl),logDir) ["configure","--enable-tests", "--enable-benchmarks"]
getConfigureInput _ _ =Nothing

-- | get the run input for build
getBuildInput ::  StaticState -> CachedFileInfo -> Bool -> Maybe RunToLogInput
getBuildInput ss cfi@(CachedFileInfo (Just cbl) (Just rootDir) _) linking=
  let dirs = ssDirectories ss
      logDir = dLogsDir dirs
      opts   = (if linking
                                then []
                                else ["--ghc-option=-c"])
  in Just $ RunToLogInput (CabalBuild cfi) (pCabalPath $ ssPaths ss) rootDir ("build-" ++ dropExtension (takeFileName cbl),logDir) ("build": opts)
getBuildInput _ _ _ =Nothing


-- | parse cabal error messages and transform them in notes
parseCabalMessages ::StaticState -> CachedFileInfo
        -> String -- ^ error output
        -> [BWNote]
parseCabalMessages ss (CachedFileInfo (Just cbl) _ _) s=let
        cf = takeFileName cbl
        cabalExe = takeFileName $ pCabalPath $ ssPaths ss
        (m,ls)=foldl (parseCabalLine cf cabalExe) (Nothing,[]) $ lines s
        in nub $ case m of
                Nothing -> ls
                Just (bwn,msgs)->ls++[makeNote bwn msgs]
        where
                parseCabalLine :: FilePath -> FilePath -> (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseCabalLine cf cabalExe (currentNote,ls) l
                        | "Error:" `isPrefixOf` l=(Just (BWNote BWError cf (mkEmptySpan cf 1 1),[dropWhile isSpace $ drop 6 l]),addCurrent currentNote ls)
                        | "Warning:" `isPrefixOf` l=let
                                msg=(dropWhile isSpace $ drop 8 l)
                                msg2=if cf `isPrefixOf` msg
                                        then dropWhile isSpace $ drop (length cf + 1) msg
                                        else msg
                                in (Just (BWNote BWWarning cf (mkEmptySpan cf (extractLine msg2) 1),[msg2]),addCurrent currentNote ls)
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l (not (any isBWNoteError ls))=(Just (bw,n),addCurrent currentNote ls)
                        | Just (jcn,msgs)<-currentNote=
                                if not $ null l
                                        then (Just (jcn,l:msgs),ls)
                                        else (Nothing,ls++[makeNote jcn msgs])
                        | otherwise =(Nothing,ls)
                extractLine el=let
                        (_,_,_,ls)=el =~ "\\(line ([0-9]*)\\)" :: (String,String,String,[String])
                        in if null ls
                                then 1
                                else readInt (head ls) 1
parseCabalMessages _ _ _ = []

-- | parse messages from build
parseBuildMessages ::  StaticState -> CachedFileInfo
        -> String -- ^ the build output
        -> [BWNote]
parseBuildMessages ss (CachedFileInfo (Just cbl) (Just root) _) s=let
        cf = takeFileName cbl
        cabalExe = takeFileName $ pCabalPath $ ssPaths ss
        distDir = getDistDir root
        (m,ls)=foldl (parseBuildLine cf cabalExe distDir) (Nothing,[]) $ lines s
        in (nub $
           case m of
               Nothing -> ls
               Just (bwn, msgs) -> ls ++ [makeNote bwn msgs])
        where
                parseBuildLine :: FilePath -> FilePath -> FilePath -> (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseBuildLine cf cabalExe distDir (currentNote,ls) l
                        | Just (jcn,msgs)<-currentNote=
                                if not (null l)  && ((' ' == head l) || (')' == last l))
                                       then (Just (jcn,l:msgs),ls)
                                       else (Nothing,ls++[makeNote jcn msgs])
                        --  | Just fp<-getBuiltPath l=(currentNote,ls,fp:fps)
                        | Just n<-extractLocation cf distDir l =(Just (n,[bwnTitle n]),ls)
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l (not (any isBWNoteError ls))=(Just (bw,n),addCurrent currentNote ls)
                        | otherwise =(Nothing,ls)
                extractLocation cf distDir el=let
                        (_,_,aft,ls)=el =~ "(.+):([0-9]+):([0-9]+):" :: (String,String,String,[String])
                        in case ls of
                                (loc:line:col:[])-> Just $ BWNote BWError (dropWhile isSpace aft) (mkEmptySpan loc (readInt line 1) (readInt col 1))
                                _ -> let
                                      (_,_,_,ls2)=el =~ "(.+)(\\(.+\\)):(.+):(.+):" :: (String,String,String,[String])
                                      in case ls2 of
                                        (loc2:ext1:_:_:[])-> Just $ BWNote BWError (drop (length loc2 + length ext1 + 1) el) (mkEmptySpan (validLoc cf distDir loc2) 1 1)
                                        _     -> Nothing
parseBuildMessages _ _ _ = []

-- | get a valid path
validLoc :: FilePath -- ^ the cabal file
        -> FilePath -- ^ the dist dir
        -> FilePath
        -> FilePath
validLoc cf distDir f=if distDir `isPrefixOf` f
        then cf
        else f

 -- | add a note with a potential additional message
addCurrent :: Maybe (BWNote, [String]) -> [BWNote] -> [BWNote]
addCurrent Nothing xs=xs
addCurrent (Just (n,msgs)) xs=xs++[makeNote n msgs]

-- | get the setup exe file name
setupExe :: FilePath -- ^ path to cabal executable
        -> FilePath
setupExe cabalExe=addExtension "setup" $ takeExtension cabalExe


-- | drop all potential prefixes from the given string
dropPrefixes :: [String] -> String -> Maybe String
dropPrefixes prfxs s2=foldr (stripPrefixIfNeeded s2) Nothing prfxs

-- | parse a Cabal error line
cabalErrorLine :: FilePath -- ^ cabal file
        -> FilePath -- ^ path to cabal executable
        -> String -- ^ line
        -> Bool -- ^ first error?
        -> Maybe (BWNote,[String])
cabalErrorLine cf cabalExe l fstErr
        | Just s4 <- dropPrefixes [cabalExe,setupExe cabalExe] l=
                                let
                                        s2=dropWhile isSpace $ drop 1 s4 -- drop 1 for ":" that follows file name
                                in if "At least the following" `isPrefixOf` s2
                                                then Just (BWNote BWError cf (mkEmptySpan cf 1 1), [s2])
                                                else
                                                        let
                                                                (loc1,_)=span (/= '\'') s2
                                                                (loc2,rest2)=span (/= ':') s2
                                                                (loc,rest)=if length loc1<length loc2 then (s2,"") else (loc2,rest2)
                                                                (realloc,line,msg)=if null rest || ":"==rest
                                                                        then    (cf,"1",s2)
                                                                        else
                                                                                let tr=tail rest
                                                                                    (line',msg')=span (/= ':') tr
                                                                                in if null msg'
                                                                                        then (loc,"1",tr)
                                                                                        else if readInt line' (-1)==(-1)
                                                                                                then (cf,"1",s2)
                                                                                                else (loc,line',tail msg')
                                                        in Just (BWNote BWError cf (mkEmptySpan realloc (readInt line 1) 1),[msg])
         | not fstErr=cabalErrorLine cf cabalExe (cabalExe ++ ":" ++ l) False
         | otherwise=Nothing

-- | read an integer and return a default value if not readable
readInt :: String -> Int -> Int
readInt s def=let parses=reads s ::[(Int,String)]
        in if null parses
                then def
                else fst $ head parses

-- | read a list and return the empty list if not readable
tryReadList :: Read a => String -> [a]
tryReadList s=let parses=reads s
        in if null parses
                then []
                else fst $ head parses

-- | add a message to the note
makeNote :: BWNote  -- ^ original note
        -> [String] -- ^ message lines
        ->BWNote
makeNote bwn msgs=let
        title=dropWhile isSpace $ unlines $ reverse msgs
        in if "Warning:" `isPrefixOf` title
                then bwn{bwnTitle=dropWhile isSpace $ drop 8 title,bwnStatus=BWWarning}
                else bwn{bwnTitle=title}

-- | stop prefix if the given string starts by it
stripPrefixIfNeeded :: String -> String -> Maybe String -> Maybe String
stripPrefixIfNeeded _ _ j@(Just _)=j
stripPrefixIfNeeded s3 prfx  _=stripPrefix prfx s3

-- | get the path of a file getting compiled
getBuiltPath :: String -- ^ the message line
        -> Maybe FilePath -- ^ the path if we could parse it
getBuiltPath line=let
         (_,_,_,ls)=line =~ "\\[[0-9]+ of [0-9]+\\] Compiling .+\\( (.+), (.+)\\)" :: (String,String,String,[String])
         in case ls of
                (src:_:[])->Just src
                _ -> Nothing
