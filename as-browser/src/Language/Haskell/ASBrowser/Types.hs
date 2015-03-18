{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Language.Haskell.ASBrowser.Types where

import Control.Arrow
import Control.Monad
import Data.Data 
import Data.SafeCopy hiding (Version)
import Data.Text hiding (empty)
import Distribution.Version
import Data.IxSet
import Data.Time
import Data.Default
import Data.String
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)

newtype PackageName = PackageName {unPkgName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

instance IsString PackageName where
  fromString = PackageName . pack

newtype PackageNameCI = PackageNameCI {unPkgNameCI :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

textToPackageNameCI :: Text -> PackageNameCI
textToPackageNameCI = PackageNameCI . toLower

textTupleToPackageNameCI :: (Text,Text) -> (PackageNameCI,PackageNameCI)
textTupleToPackageNameCI = join (***) textToPackageNameCI

deriveSafeCopy 0 'base ''PackageName
deriveSafeCopy 0 'base ''PackageNameCI

deriveSafeCopy 0 'base ''Version
deriveSafeCopy 0 'base ''VersionRange

instance IsString Version where
  fromString = fst . Prelude.head .readP_to_S parseVersion

data Local = Local | Packaged
  deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''Local

data Expose = Exposed | Included | Main
  deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''Expose


data PackageMetaData = PackageMetaData
  { pkgMDAuthor :: Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageMetaData

instance Default PackageMetaData where
  def = PackageMetaData ""

newtype Doc = Doc {unDoc :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)
    
instance Default Doc where
  def = Doc ""    
    
deriveSafeCopy 0 'base ''Doc

newtype ComponentName = ComponentName {unCompName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentName


instance IsString ComponentName where
  fromString = ComponentName . pack

data ComponentType = Library | Executable | Test | BenchMark
    deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentType

data Component = Component
 { cName :: ComponentName
 , cType :: ComponentType
 } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Component

newtype ModuleName = ModuleName {unModName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleName

instance IsString ModuleName where
  fromString = ModuleName . pack



newtype DeclName = DeclName {unDeclName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''DeclName

data DeclType = DeclData | DeclNewType | DeclClass | DeclInstance | DeclType 
    | DeclDataFamily | DeclTypeFamily | DeclDataInstance | DeclTypeInstance
    deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''DeclType

data WriteDate = WriteDate
  { wdCreated :: UTCTime
  , wdUpdated :: UTCTime
  } deriving (Show, Read, Eq, Ord, Typeable,Data)
  

deriveSafeCopy 0 'base ''WriteDate


data PackageKey = PackageKey 
  { pkgName       :: PackageName
  , pkgVersion    :: Version
  , pkgLocal      :: Local
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageKey
  

data Package = Package 
  { pkgKey        :: PackageKey
  , pkgDoc        :: Doc
  , pkgMeta       :: PackageMetaData
  , pkgComponents :: [Component]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Package

instance Indexable Package where
  empty = ixSet
    [ ixFun $ \pkg -> [ pkgKey pkg ]
    , ixFun $ \pkg -> [ textToPackageNameCI $ unPkgName $ pkgName $ pkgKey pkg ]
    ]

data ModuleKey = ModuleKey 
  { modName       :: ModuleName
  , modPackageKey :: PackageKey
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleKey

data Module = Module
  { modKey        :: ModuleKey
  , modDoc        :: Doc
  , modExpose     :: Expose
  , modComponents :: [Component]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Module

data ComponentKey = ComponentKey
  { cPackageKey :: PackageKey
  , cComponent  :: Component
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentKey

instance Indexable Module where
  empty = ixSet
    [ ixFun $ \mo -> [ modPackageKey $ modKey mo ]
    , ixFun $ \mo -> Prelude.map (ComponentKey (modPackageKey $ modKey mo)) $ modComponents mo 
    , ixFun $ \mo -> [ modName $ modKey mo ]
    , ixFun $ \mo -> [ modKey mo ]
    ]

data Database = Database
  { dPackages :: IxSet Package
  , dModules  :: IxSet Module
  } deriving (Data, Typeable)

deriveSafeCopy 0 'base ''Database

instance Default Database where
  def = Database empty empty
