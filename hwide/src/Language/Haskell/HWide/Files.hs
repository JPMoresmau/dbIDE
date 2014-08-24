{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.HWide.Files where

import qualified Data.Map as DM
import Data.Typeable (Typeable)

data EditorState = EditorState 
  { esFileInfos :: DM.Map FilePath FileInfo 
  } deriving (Show,Read,Eq,Ord,Typeable)

data FileInfo = FileInfo
  { fiFile  :: FilePath
  , fiDirty :: Bool
  } deriving (Show,Read,Eq,Ord,Typeable)
  
setDirty :: FileInfo -> FileInfo
setDirty fi = fi {fiDirty=True}

setClean :: FileInfo -> FileInfo
setClean fi = fi {fiDirty=False}

mkEditorState :: EditorState
mkEditorState = EditorState $ DM.singleton "" (FileInfo "" False)


addFile :: FilePath -> EditorState -> EditorState
addFile fp es = es{esFileInfos=DM.insert fp (FileInfo fp False) $ esFileInfos es}


removeFile:: FilePath -> EditorState -> EditorState
removeFile fp es = es{esFileInfos=DM.delete fp $ esFileInfos es}


adjustFile :: FilePath -> (FileInfo -> FileInfo) -> EditorState -> EditorState
adjustFile fp f es = es{esFileInfos=DM.adjust f fp $ esFileInfos es}

