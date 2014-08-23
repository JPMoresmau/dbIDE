{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.HWide.UI.FileList where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath              (takeFileName)
import           Data.List                    (sortBy, nub, elemIndex)

import           Control.Monad                (forM, void)
import           Data.Ord                     (comparing)
import Data.Typeable (Typeable)

data FileList = FileList 
  { flList  :: Element 
  , flbSelection :: Behavior FilePath
  }

instance Widget FileList where
  getElement = flList

data FileListData = FileListData
  { fldCurrent :: FilePath
  , fldList    :: [FilePath]  
  } deriving (Show,Read,Eq,Ord,Typeable)

addFileToList :: FilePath -> FileListData -> FileListData
addFileToList fp (FileListData _ cs) = FileListData fp $ nub $ sortBy (comparing takeFileName) (fp : cs)

removeFileFromList :: FileListData -> FileListData
removeFileFromList (FileListData c cs) = let
  ncs = filter (c /=) cs
  nc = if null ncs then "" else head ncs
  in FileListData nc ncs

setCurrentFileInList :: FilePath -> FileListData -> FileListData
setCurrentFileInList fp (FileListData _ cs) = FileListData fp cs

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
    
  bData <- accumB (FileListData "" []) eAll
  onChanges bData setFs

  return $ FileList sel (fldCurrent <$> bData)
  