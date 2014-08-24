{-# LANGUAGE DeriveDataTypeable #-}
-- | Edited files handling
module Language.Haskell.HWide.Files where

import qualified Data.Map as DM
import Data.Typeable (Typeable)

-- | The state we keep in the editor
data EditorState = EditorState 
  { esFileInfos :: DM.Map FilePath FileInfo 
  } deriving (Show,Read,Eq,Ord,Typeable)


-- | Information about a file
data FileInfo = FileInfo
  { fiFile  :: FilePath -- ^ File path
  , fiDirty :: Bool -- ^ dirty -> file has been modified and not saved to disk
  } deriving (Show,Read,Eq,Ord,Typeable)
  

-- | Set a file as dirty
setDirty :: FileInfo -> FileInfo
setDirty fi = fi {fiDirty=True}


-- | Set a file as clean
setClean :: FileInfo -> FileInfo
setClean fi = fi {fiDirty=False}


-- | Create a new Editor state
mkEditorState :: EditorState
mkEditorState = EditorState $ DM.singleton "" (FileInfo "" False)


-- | Add a new file to the state
addFile :: FilePath -> EditorState -> EditorState
addFile fp es = es{esFileInfos=DM.insert fp (FileInfo fp False) $ esFileInfos es}


-- | Remove a file from the state
removeFile:: FilePath -> EditorState -> EditorState
removeFile fp es = es{esFileInfos=DM.delete fp $ esFileInfos es}


-- | Perform an operation on the FileInfo associated with the given file
adjustFile :: FilePath -> (FileInfo -> FileInfo) -> EditorState -> EditorState
adjustFile fp f es = es{esFileInfos=DM.adjust f fp $ esFileInfos es}

