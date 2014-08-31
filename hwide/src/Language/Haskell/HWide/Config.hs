{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Edited files handling
module Language.Haskell.HWide.Config where

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
import Data.Maybe (fromMaybe)

-- | The state we keep in the editor
data EditorState = EditorState 
  { esFileInfos :: DM.Map FilePath FileInfo 
  , esCurrent   :: FilePath
  } deriving (Show,Read,Eq,Ord,Typeable)

-- | Default value
instance Default EditorState where
  def = EditorState (DM.singleton "" (defFileInfo "")) ""

-- | Reading from JSON/YAML
instance FromJSON EditorState where
  parseJSON (Object v) = EditorState
    <$> ((DM.fromList . map (\x->(x,defFileInfo x)) . ("":) . fromMaybe []) <$> v .:? "files")
    <*> (fromMaybe "" <$> v .:? "current")
  parseJSON _ = fail "EditorState"

-- | Writing to JSON/YAML
instance ToJSON EditorState where
  toJSON (EditorState fis c)=object ["files" .= filter (not . null) (DM.keys fis), "current" .= c]

-- | Information about a file
data FileInfo = FileInfo
  { fiFile  :: FilePath -- ^ File path
  , fiDirty :: Bool -- ^ dirty -> file has been modified and not saved to disk
  } deriving (Show,Read,Eq,Ord,Typeable)
  
defFileInfo :: FilePath -> FileInfo
defFileInfo fp = FileInfo fp False

-- | Set a file as dirty
setDirty :: FileInfo -> FileInfo
setDirty fi = fi {fiDirty=True}


-- | Set a file as clean
setClean :: FileInfo -> FileInfo
setClean fi = fi {fiDirty=False}


-- | get the configuration file
getConfigFile :: FilePath -> FilePath
getConfigFile cd = cd </> "hwide.yaml"


-- | Create a new Editor state, reading from the config state if necessary
mkEditorState :: FilePath -> IO EditorState
mkEditorState cd = do
  let cf = getConfigFile cd
  ex <- doesFileExist cf
  if ex 
    then do
      bs <- B.readFile cf
      let eES = decodeEither bs
      case eES of
        Right es -> do
          remove <- filterM (liftM not . doesFileExist) $ filter (not . null) $ DM.keys $ esFileInfos es
          let es2 = foldr removeFile es remove
          let c2 = if esCurrent es2 `elem` remove
                      then 
                        let fs = filter (not . null) $ DM.keys $ esFileInfos es2
                        in if null fs then "" else head fs
                      else esCurrent es2
          return es2{esCurrent=c2}
        Left s -> do
          putStrLn s
          return def
    else
      return def


-- | save Editor State
saveEditorState :: FilePath -> EditorState -> IO()
saveEditorState cd es = do
  let cf = getConfigFile cd
  let bs = encode es
  B.writeFile cf bs


-- | Add a new file to the state
addFile :: FilePath -> EditorState -> EditorState
addFile fp es = setCurrent fp $ es{esFileInfos=DM.insert fp (FileInfo fp False) $ esFileInfos es}


-- | Remove a file from the state
removeFile:: FilePath -> EditorState -> EditorState
removeFile fp es = es{esFileInfos=DM.delete fp $ esFileInfos es}


-- | Perform an operation on the FileInfo associated with the given file
adjustFile :: FilePath -> (FileInfo -> FileInfo) -> EditorState -> EditorState
adjustFile fp f es = es{esFileInfos=DM.adjust f fp $ esFileInfos es}

-- | Set the current file
setCurrent :: FilePath -> EditorState -> EditorState
setCurrent fp es = es{esCurrent = fp}
