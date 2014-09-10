module Language.Haskell.HWide.Notes where


-- | status of notes: error or warning
data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
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
        deriving (Show,Read,Eq)


-- | build an empty span in a given file at a given location
mkEmptySpan :: FilePath -> Int -> Int -> BWLocation
mkEmptySpan src line col = BWLocation src line col line col

-- | a note on a source file
data BWNote=BWNote {
        bwnStatus :: BWNoteStatus -- ^ status of the note
        ,bwnTitle :: String -- ^ message
        ,bwnLocation :: BWLocation -- ^ where the note is
        }
        deriving (Show,Read,Eq)


-- | is a note an error?      
isBWNoteError :: BWNote -> Bool
isBWNoteError bw=bwnStatus bw == BWError
        
   