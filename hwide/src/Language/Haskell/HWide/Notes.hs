{-# LANGUAGE DeriveDataTypeable #-}
-- | Notes about errors and warnings and their location
module Language.Haskell.HWide.Notes where

import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Default
import Data.Typeable
import System.FilePath
import Text.Printf

-- | All notes
data Notes = Notes
  {
    nByFiles :: DM.Map FilePath (DS.Set BWNote) 
  , nNoteCount :: NoteCount
  } deriving (Show,Read,Eq,Typeable)

-- | Default instance
instance Default Notes where
  def = Notes def def

-- | Count of different types of notes
data NoteCount = NoteCount
  {
    ncErrors   :: Int
  , ncWarnings :: Int  
  } deriving (Show,Read,Eq,Typeable)

-- | Default instance
instance Default NoteCount where
  def = NoteCount 0 0


-- | Get all notes
allNotes :: Notes -> [BWNote]
allNotes = concatMap DS.toList . DM.elems . nByFiles

-- | Dump note counts to a string
noteCountToString :: NoteCount -> String
noteCountToString (NoteCount errs warns) = printf "%d errors, %d warnings" errs warns

-- | Add notes for a given file path 
addNotes :: FilePath -> [BWNote] -> Notes -> Notes
addNotes root bwns ns = let
  nf = foldr addBW (nByFiles ns) bwns
  in syncCount $ ns{nByFiles=nf}
  where 
    addBW bw = 
      let (fp,absBw) = makeAbsoluteLocation bw
      in DM.insertWith DS.union fp (DS.singleton absBw)
    makeAbsoluteLocation bw= 
      let loc = bwnLocation bw
          absLoc = root </> bwlSrc loc
      in (absLoc,bw{bwnLocation=loc{bwlSrc=absLoc}})

--removeNotes :: FilePath -> Notes -> Notes
--removeNotes fp ns = syncCount $ ns{nByFiles=DM.delete fp $ nByFiles ns}

-- | Remove notes for a given file path
removeNotes :: FilePath -> [FilePath] -> Notes -> Notes
removeNotes root fps ns = syncCount $ ns{nByFiles=foldr (\fp -> DM.delete (root </> fp)) (nByFiles ns) fps}

-- | Ensure counts are synchronized with actual notes
syncCount :: Notes -> Notes
syncCount n = n{nNoteCount=go $ nByFiles n}
  where 
    go = DM.foldr (flip (DS.foldr addNS)) def
    addNS b c=case bwnStatus b of
      BWError -> c{ncErrors=ncErrors c + 1}
      BWWarning -> c{ncWarnings=ncWarnings c + 1}
  
-- | status of notes: error or warning
data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq,Ord,Typeable)
 
-- | read an object from a String, with a given error message if it fails 
readObj :: Read a=> String -> String -> a
readObj msg s=let parses=reads s -- :: [(a,String)]
        in if null parses 
                then error (msg ++ ": " ++ s ++ ".")
                else fst $ head parses 
 
-- | location of a note/error (lines and columns start at 1)
data BWLocation=BWLocation {
        bwlSrc::FilePath -- ^ source file 
        ,bwlLine::Int -- ^ line
        ,bwlCol::Int -- ^ column
        ,bwlEndLine::Int -- ^ end line
        ,bwlEndCol::Int -- ^ end line
        }
        deriving (Show,Read,Eq,Ord,Typeable)


-- | build an empty span in a given file at a given location
mkEmptySpan :: FilePath -> Int -> Int -> BWLocation
mkEmptySpan src line col = BWLocation src line col line col

-- | a note on a source file
data BWNote=BWNote {
        bwnStatus :: BWNoteStatus -- ^ status of the note
        ,bwnTitle :: String -- ^ message
        ,bwnLocation :: BWLocation -- ^ where the note is
        }
        deriving (Show,Read,Eq,Ord,Typeable)


-- | is a note an error?      
isBWNoteError :: BWNote -> Bool
isBWNoteError bw=bwnStatus bw == BWError
        
   