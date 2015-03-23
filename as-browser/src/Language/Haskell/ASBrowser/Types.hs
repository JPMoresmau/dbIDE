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
  fromString = fst . Prelude.head . Prelude.filter (\(_,rest)->Prelude.null rest). readP_to_S parseVersion

data Local = Local | Packaged
  deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''Local

data Expose = Exposed | Included | Main
  deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''Expose


data PackageMetaData = PackageMetaData
  { pkgMDAuthor :: !Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageMetaData

instance Default PackageMetaData where
  def = PackageMetaData ""

data Doc = Doc 
  { dShort :: !Text
  , dLong :: !Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)
    
instance Default Doc where
  def = Doc "" ""
    
deriveSafeCopy 0 'base ''Doc

newtype ComponentName = ComponentName {unCompName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentName


instance IsString ComponentName where
  fromString = ComponentName . pack

data ComponentType = Library | Executable | Test | BenchMark
    deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentType

data PackageKey = PackageKey 
  { pkgName       :: !PackageName
  , pkgVersion    :: !Version
  , pkgLocal      :: !Local
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageKey

data PackageRef = PackageRef
  { prName :: !PackageName
  , prRange :: !VersionRange
  } deriving (Show,Read,Eq,Typeable,Data)

deriveSafeCopy 0 'base ''PackageRef

instance Ord PackageRef where
  compare (PackageRef name1 range1) (PackageRef name2 range2) = 
    case compare name1 name2 of
      EQ -> compare (show range1) (show range2)
      c  -> c

data ComponentKey = ComponentKey
  { cPackageKey :: !PackageKey
  , cName  :: !ComponentName
  } deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''ComponentKey

data Component = Component
 { cKey        :: !ComponentKey
 , cType       :: !ComponentType
 , cRefs       :: [PackageRef]
 , cExtensions :: [Text] -- don't use the Extension type since it only got Ord instance recently, etc
 } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Component


instance Indexable Component where
  empty = ixSet
    [ ixFun $ \c -> [ cPackageKey $ cKey c ]
    , ixFun $ \c -> [ cKey c ]
    , ixFun $ \c -> Prelude.map prName $ cRefs c
    ]

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



data Package = Package 
  { pkgKey        :: !PackageKey
  , pkgDoc        :: !Doc
  , pkgMeta       :: !PackageMetaData
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Package



instance Indexable Package where
  empty = ixSet
    [ ixFun $ \pkg -> [ pkgKey pkg ]
    , ixFun $ \pkg -> [ pkgName $ pkgKey pkg ]
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
  , modComponents :: [ComponentName]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Module


instance Indexable Module where
  empty = ixSet
    [ ixFun $ \mo -> [ modPackageKey $ modKey mo ]
    , ixFun $ \mo -> Prelude.map (ComponentKey $ modPackageKey $ modKey mo) $ modComponents mo 
    , ixFun $ \mo -> [ modName $ modKey mo ]
    , ixFun $ \mo -> [ modKey mo ]
    ]

data FullPackage = FullPackage
  { fpPackage :: !Package
  , fpComponents :: ![Component]
  , fpModules :: ![Module]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''FullPackage

data Database = Database
  { dPackages :: IxSet Package
  , dComponents :: IxSet Component
  , dModules  :: IxSet Module
  } deriving (Data, Typeable)

deriveSafeCopy 0 'base ''Database

instance Default Database where
  def = Database empty empty empty

