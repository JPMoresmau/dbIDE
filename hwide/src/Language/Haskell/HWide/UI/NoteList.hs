-- | List of notes (warnings or errors)
module Language.Haskell.HWide.UI.NoteList where

import qualified Graphics.UI.Threepenny as UI 
import           Graphics.UI.Threepenny.Core

import           Language.Haskell.HWide.Notes 
import           Data.List                    
import           System.FilePath              

-- | Note list widget
data NoteList = NoteList 
  { nlList  :: Element -- ^ The UI element 
  }
  

-- | Widget instance
instance Widget NoteList where
  getElement = nlList
  
-- | Build widget
mkNoteList 
  :: Behavior Notes -- ^ Behavior of full state (with lists of open file)
  -> Handler BWNote  -- ^ Handler for note selection
  -> FilePath -- ^ root dir
  -> UI NoteList
mkNoteList bNotes fireSelect rootDir = do
  elErrList <- UI.div #. "noteList"
  onChanges bNotes $ \ns -> do
    els <- mkListContents ns fireSelect rootDir
    element elErrList # set children els
    
  return $ NoteList elErrList
  
-- | Build list element
mkListContents :: Notes -> Handler BWNote -> FilePath -> UI [Element]
mkListContents ns fireSelect rootDir = do
  let fpns=sort $ allNotes ns
  mapM (mkListElement fireSelect rootDir) fpns

-- | build one element
mkListElement :: Handler BWNote -> FilePath -> BWNote -> UI Element
mkListElement fireSelect rootDir n = do
  s <- UI.span  
      # set html (intercalate "<br/>" (lines $ bwnTitle n) ++ "<br/>")
      #. noteCls (bwnStatus n) 
      # set UI.title__ (makeRelative rootDir (bwlSrc $ bwnLocation n) ++ ":" ++ show (bwlLine $ bwnLocation n)) 
  on UI.click s $ const $ liftIO $ fireSelect n
  return s
  
-- | get the class from the status
noteCls :: BWNoteStatus -> String
noteCls BWError = "noteError"
noteCls BWWarning = "noteWarning"
