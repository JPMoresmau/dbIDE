{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Utility functions
module Language.Haskell.HWide.Util where

import System.Directory 
import System.FilePath 
import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import Data.Char (toLower)
import Paths_hwide
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding 
import Data.Hashable (hash)
import System.IO
import System.Process
import System.Exit (ExitCode(..))

-- | A File System item, wrapping the path
data FSItem = 
    -- | A directory
    Dir 
      {fsiPath :: FilePath}
  | -- | A path
    File 
      {fsiPath :: FilePath}
  deriving (Show,Read,Eq,Typeable)

-- | Instance for sorting
instance Ord FSItem where
  (Dir _)  <= (File _) = True
  (File _) <= (Dir _)  = False
  f1       <= f2       = can f1 <= can f2
    where can = map toLower . takeFileName . fsiPath


-- | List all files inside a given directory
listFiles :: FilePath -> IO [FSItem]
listFiles cd = do
  fs <- getDirectoryContents cd
  let visible = filter (not . ("." `isPrefixOf`) . takeFileName) fs
  mapM tofs visible
  where
    tofs fp = do
      let full = cd </> fp
      isFile <- doesFileExist full
      can <- canonicalizePath full
      return $ if isFile
        then File can
        else Dir can
        

-- | Get directory where static resources are kept
getStaticDir :: IO FilePath
getStaticDir = do
  d <- Paths_hwide.getDataFileName "wwwroot"
  ex <- doesDirectoryExist d
  if ex 
    then return d
    else do
      cd <- getCurrentDirectory
      return $ cd </> "wwwroot"


-- | Get the CodeMirror file for the given text
getMode :: FilePath -> T.Text
getMode fp = case takeExtension fp of
  ".hs"  -> "haskell"
  ".lhs" -> "haskell"
  _      -> "text"


-- | Get the file contents as a Text. Assumes UTF-8 encoding
getFileContents :: FilePath -> IO T.Text
getFileContents fp = do
  bs <- B.readFile fp
  return $ decodeUtf8 bs


-- | Set the file contents as a Text. Assumes UTF-8 encoding
setFileContents :: FilePath -> T.Text -> IO ()
setFileContents fp cnts = 
  B.writeFile fp $ encodeUtf8 cnts


-- | Get the directory where hwide is going to store its info
getHWideDir ::  IO FilePath
getHWideDir = do
  userDir <- getAppUserDataDirectory "hwide"
  createDirectoryIfMissing False userDir
  return userDir
  
-- | Get the workspace specific directory
-- This is inside the app directory to not pollute the user's workspace
getHWideWorkspaceDir :: FilePath -> IO FilePath
getHWideWorkspaceDir root = do
  userDir <- getHWideDir
  let rootName = hash root 
  createSubDir userDir $ show rootName


getLogsDir :: FilePath -> IO FilePath
getLogsDir root = createSubDir root "logs"
  
  
createSubDir :: FilePath -> FilePath -> IO FilePath
createSubDir root dir = do
  let subDir = root </> makeValid dir
  createDirectoryIfMissing True subDir
  return subDir

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile fp = do
  let dir = takeDirectory fp
  go dir
  where 
    go dir = do
      fs <- getDirectoryContents dir
      let cabals = filter ((".cabal" ==) . takeExtension) fs 
      case cabals of
        [x] -> return $ Just x
        []  -> do
          let up = takeDirectory dir
          if up == dir then return Nothing else go up
        xs  -> do
          let sm = filter ((takeFileName dir ==) . dropExtension . takeFileName) xs
          case sm of
            (x:_) -> return $ Just x
            _      -> return $ Just $ head xs


data Directories = Directories
  { dRootDir    :: FilePath
  , dWorkDir    :: FilePath
  , dLogsDir    :: FilePath
  , dSandboxDir :: FilePath
  } deriving (Read,Show,Eq,Ord,Typeable)
        

data RunToLogResult = RunToLogResult
  {
    rtlrOutFile  :: FilePath
  , rtlrErrFile  :: FilePath
  , rtlrExitCode :: ExitCode
  } deriving (Read,Show,Eq,Ord,Typeable)


-- | Run a program in a directory and arguments, writing output to the log file
runToLog :: FilePath -> FilePath -> (String,FilePath) -> [String] -> IO RunToLogResult
runToLog pgm dir (logName,logDir) args = do
  let outF = logDir </> addExtension (logName ++ "-out") "log"
  let errF = logDir </> addExtension (logName ++ "-err") "log"
  ex <- withFile outF WriteMode $ \outH ->
   withFile errF WriteMode $ \errH -> do
    let cp = (proc pgm args)
              {
                cwd = Just dir
              , std_out = UseHandle outH
              , std_err = UseHandle errH  
              }
    (_,_,_,ph) <- createProcess cp
    waitForProcess ph
  return $ RunToLogResult outF errF ex
