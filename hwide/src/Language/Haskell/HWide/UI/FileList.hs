{-# LANGUAGE DeriveDataTypeable #-}
-- | List of opened files
module Language.Haskell.HWide.UI.FileList where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath              (takeFileName)
import           Data.List                    (sortBy, nub, elemIndex)

import           Control.Monad                (forM, void)
import           Data.Ord                     (comparing)
import Data.Typeable (Typeable)

-- | File list widget
data FileList = FileList 
  { flList  :: Element -- ^ The UI element 
  , flbSelection :: Behavior FilePath -- ^ The currently selected file
  }

-- | Widget instance
instance Widget FileList where
  getElement = flList

-- | The state we encapsulate
data FileListData = FileListData
  { fldCurrent :: FilePath -- ^ Current file
  , fldList    :: [FilePath] -- ^ All opened files
  } deriving (Show,Read,Eq,Ord,Typeable)


-- | Add a file to the list
addFileToList :: FilePath -> FileListData -> FileListData
addFileToList fp (FileListData _ cs) = FileListData fp $ nub $ sortBy (comparing takeFileName) (fp : cs)

-- | Remove a file from the list
removeFileFromList :: FileListData -> FileListData
removeFileFromList (FileListData c cs) = let
  ncs = filter (c /=) cs
  nc = if null ncs then "" else head ncs
  in FileListData nc ncs

-- | Set current file
setCurrentFileInList :: FilePath -> FileListData -> FileListData
setCurrentFileInList fp (FileListData _ cs) = FileListData fp cs


-- | Build widget
fileList :: Event FilePath -> Event () -> UI FileList
fileList eOpen eClose = do  
  sel <- UI.select  #. "fileList"
  let
    setFs :: FileListData -> UI()
    setFs (FileListData c fs) = do
      opts <- forM fs (\fp -> UI.option # set value fp # set text (takeFileName fp))
      void $ return sel # set children opts # set UI.selection (elemIndex c fs)
    eSetCurrent = setCurrentFileInList <$> UI.selectionValueChange sel
    eAdd = addFileToList <$> eOpen
    eRemove = const removeFileFromList <$> eClose
    eAll = concatenate <$> unions [eSetCurrent,eAdd,eRemove]
  -- behavior
  bData <- accumB (FileListData "" []) eAll
  -- we need the full value to build the list, hence we listen on the behavior value change
  onChanges bData setFs

  return $ FileList sel (fldCurrent <$> bData)
  