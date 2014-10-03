{-# LANGUAGE DeriveDataTypeable #-}
-- | The types we use
-- A lot of this work is Copyright Neil Mitchell 2014. See <https://github.com/ndmitchell/ghcid>
module Language.Haskell.Ghci.Types where

import Data.Typeable

type GhciExec = String -> IO (Either GhciError [String])

data GhciError = UnexpectedExit String String
  deriving (Show,Eq,Ord,Typeable)


data Severity = Warning | Error 
  deriving (Show,Eq,Ord,Bounded,Enum,Typeable)


data Load
    = Loading {loadModule :: String, loadFile :: FilePath}
    | Message
        {loadSeverity :: Severity
        ,loadFile :: FilePath
        ,loadFilePos :: (Int,Int)
        ,loadMessage :: [String]
        }
      deriving (Show,Eq)
      

--isMessage Message{} = True
--isMessage _ = False