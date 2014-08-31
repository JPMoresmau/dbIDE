{-# LANGUAGE DeriveDataTypeable #-}
-- | List of opened files
module Language.Haskell.HWide.UI.FileList where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath              (takeFileName)
import           Data.List                    (elemIndex)

import           Control.Monad                (forM, void)
import qualified Data.Map as DM
import Language.Haskell.HWide.Config

-- | File list widget
data FileList = FileList 
  { flList  :: Element -- ^ The UI element 
--  , flbSelection :: Behavior FilePath -- ^ The currently selected file
  }

-- | Widget instance
instance Widget FileList where
  getElement = flList
--
---- | The state we encapsulate
--data FileListData = FileListData
--  { fldCurrent :: FilePath -- ^ Current file
--  , fldList    :: [FilePath] -- ^ All opened files
--  } deriving (Show,Read,Eq,Ord,Typeable)
--
---- | Default list
--instance Default FileListData where
--  def = FileListData "" []
--
---- | Add a file to the list
--addFileToList :: FilePath -> FileListData -> FileListData
--addFileToList fp (FileListData _ cs) = FileListData fp $ nub $ sortBy (comparing takeFileName) (fp : cs)
--
---- | Remove a file from the list
--removeFileFromList :: FileListData -> FileListData
--removeFileFromList (FileListData c cs) = let
--  ncs = filter (c /=) cs
--  nc = if null ncs then "" else head ncs
--  in FileListData nc ncs
--
---- | Set current file
--setCurrentFileInList :: FilePath -> FileListData -> FileListData
--setCurrentFileInList fp (FileListData _ cs) = FileListData fp cs


-- | Build widget
fileList 
  :: Behavior EditorState -- ^ Behavior of full state (with lists of open file)
  -> Handler FilePath  -- ^ Handler for current file change
  -> UI FileList
fileList bEs fireModify = do  
  sel <- UI.select  #. "fileList"
  let
    getFileText :: FileInfo -> String
    getFileText fi = takeFileName $ fiFile fi ++ (if fiDirty fi then "*" else "")
    setFs :: EditorState -> UI()
    setFs (EditorState mfs c) = do
      let fs = DM.assocs mfs
      opts <- forM (map snd fs) (\fi -> UI.option # set value (fiFile fi) # set text (getFileText fi))
      void $ return sel # set children opts # set UI.selection (elemIndex c (map fst fs))
  on UI.selectionValueChange sel $ liftIO . fireModify
  initFld <- currentValue bEs
  setFs initFld
  -- we need the full value to build the list, hence we listen on the behavior value change
  onChanges bEs setFs

  return $ FileList sel
  