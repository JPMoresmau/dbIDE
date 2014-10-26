{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
-- | Configuration data
module Language.Haskell.HWide.Config where

import Data.List (delete, nub)
import qualified Data.Map as DM
import Data.Typeable (Typeable)
import Data.Yaml
import qualified Data.ByteString as B
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Default
import Data.Functor
import Control.Applicative
import Control.Monad (filterM, liftM)
import Graphics.UI.Threepenny.Core (MonadIO,liftIO)

import Language.Haskell.HWide.Util
import Language.Haskell.HWide.Internal.UniqueQueue
import Reactive.Threepenny (Event)

-- | State that doesn't changed during a session
data StaticState = StaticState
  { ssPaths         :: Paths
  , ssDirectories   :: Directories
  , ssRunQueue      :: UniqueQueue RunToLogInput
  , ssRunEvent      :: Event (RunToLogInput,RunToLogResult)
  } deriving (Typeable)

-- | Config that changeable by the user
data Configuration = Config
  { cPaths :: Paths
  } deriving (Read,Show,Eq,Ord,Typeable)

-- | Default value
instance Default Configuration where
  def = Config def

-- | Reading from JSON/YAML
instance FromJSON Configuration where
  parseJSON (Object v) = Config
    <$> v .:? "paths" .!= def
  parseJSON _ = fail "Paths"

-- | Writing to JSON/YAML
instance ToJSON Configuration where
  toJSON (Config p)=object ["paths" .= p]

-- | Get the user modifiable configuration file
getConfigFile :: FilePath -> FilePath
getConfigFile rootDir = rootDir </> ".hwide.yaml"

-- | Read the configuration
readConfig :: FilePath -> IO Configuration
readConfig rootDir = do
  let cFile = getConfigFile rootDir
  readYAML cFile

-- | Generic read YAML object, returning default if file is not found, etc.
readYAML :: (Default b, FromJSON b) => FilePath -> IO b
readYAML f= do
  ex <- doesFileExist f
  if ex 
    then do
      eC <- decodeEither <$> B.readFile f
      case eC of
        Right c -> return c
        Left s -> do
          writeToOut s
          return def
    else return def

-- | Useful paths
data Paths = Paths
  { pCabalPath   :: FilePath
  , pSandboxPath :: Maybe FilePath
  } deriving (Read,Show,Eq,Ord,Typeable)

-- | Default value
instance Default Paths where
  def = Paths "cabal" Nothing

-- | Reading from JSON/YAML
instance FromJSON Paths where
  parseJSON (Object v) = Paths
    <$> v .:? "cabal" .!= def
    <*> v .:? "sandbox"
  parseJSON _ = fail "Paths"

-- | Writing to JSON/YAML
instance ToJSON Paths where
  toJSON (Paths cb sb)=object ["cabal" .= cb, "sandbox" .= sb]

-- | The state we keep in the editor
data EditorState = EditorState 
  { esFileInfos :: DM.Map FilePath FileInfo 
  , esCurrent   :: [FilePath]
  } deriving (Read,Show,Eq,Ord,Typeable)

-- | Default value
instance Default EditorState where
  def = EditorState (DM.singleton "" (defFileInfo "")) [""]

-- | Reading from JSON/YAML
instance FromJSON EditorState where
  parseJSON (Object v) = EditorState
    <$> ((DM.fromList . map (\x->(x,defFileInfo x)) . ("":)) <$> v .:? "files" .!= [])
    <*> (v .:? "files" .!= [""])
  parseJSON _ = fail "EditorState"

-- | Writing to JSON/YAML
instance ToJSON EditorState where
  toJSON (EditorState _ c)=object ["files" .= c]

-- | Information about a file
data FileInfo = FileInfo
  { fiFile  :: FilePath -- ^ File path
  , fiDirty :: Bool -- ^ dirty -> file has been modified and not saved to disk
  } deriving (Show,Read,Eq,Ord,Typeable)
  
-- | Default file info for a given file
defFileInfo :: FilePath -> FileInfo
defFileInfo fp = FileInfo fp False

-- | Set a file as dirty
setDirty :: FileInfo -> FileInfo
setDirty fi = fi {fiDirty=True}


-- | Set a file as clean
setClean :: FileInfo -> FileInfo
setClean fi = fi {fiDirty=False}


-- | get the configuration file
getStateFile :: FilePath -> FilePath
getStateFile cd = cd </> "hwide-state.yaml"


-- | Create a new Editor state, reading from the config state if necessary
mkEditorState :: FilePath -> IO EditorState
mkEditorState rootDir = do
  let cf = getStateFile rootDir
  es <- readYAML cf
  remove <- filterM (liftM not . doesFileExist) $ filter (not . null) $ DM.keys $ esFileInfos es
  let es2 = foldr removeFile es remove
      c2  = if head (esCurrent es2) `elem` remove
              then 
                let fs = filter (not . null) $ DM.keys $ esFileInfos es2
                in if null fs then "" else head fs
              else head $ esCurrent es2
  return es2{esCurrent=nub [c2,""]}


-- | save Editor State
saveEditorState :: FilePath -> EditorState -> IO()
saveEditorState cd es = do
  let cf = getStateFile cd
  let bs = encode es
  B.writeFile cf bs


-- | Add a new file to the state
addFile :: FilePath -> EditorState -> EditorState
addFile fp es = es{esFileInfos=DM.insertWith (curry snd) fp (FileInfo fp False) $ esFileInfos es}


-- | Remove a file from the state
removeFile:: FilePath -> EditorState -> EditorState
removeFile fp es@EditorState{..}  = es{esFileInfos=DM.delete fp esFileInfos,
  esCurrent = delete fp esCurrent}


-- | Perform an operation on the FileInfo associated with the given file
adjustFile :: FilePath -> (FileInfo -> FileInfo) -> EditorState -> EditorState
adjustFile fp f es = es{esFileInfos=DM.adjust f fp $ esFileInfos es}

-- | Set the current file
setCurrent :: FilePath -> EditorState -> EditorState
setCurrent fp es@EditorState{..} = es{esCurrent = fp : delete fp esCurrent}

scheduleRun :: (MonadIO m) => StaticState -> RunToLogInput -> m ()
scheduleRun ss = liftIO . pushUnique (ssRunQueue ss)