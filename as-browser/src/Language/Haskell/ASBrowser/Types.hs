{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Language.Haskell.ASBrowser.Types where

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad
import Data.Data 
import Data.SafeCopy hiding (Version)
import Data.Text hiding (empty)
import Distribution.Version
import Distribution.Text (simpleParse, display)
import Data.IxSet
import Data.Time
import Data.Default
import Data.String
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Maybe

import Data.Aeson
import Data.Aeson.TH

newtype PackageName = PackageName {unPkgName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''PackageName)

instance IsString PackageName where
  fromString = PackageName . pack

newtype PackageNameCI = PackageNameCI {unPkgNameCI :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''PackageNameCI)

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

instance ToJSON Version where
  toJSON = String . pack . showVersion 

instance FromJSON Version where
  parseJSON (String v)=pure $ fromString $ unpack v
  parseJSON _ = fail "Version"

instance IsString VersionRange where
  fromString = fromJust . simpleParse
  
instance ToJSON VersionRange where
  toJSON = String . pack . display

instance FromJSON VersionRange where
  parseJSON (String v)=pure $ fromString $ unpack v
  parseJSON _ = fail "VersionRange"

data Local = Local | Packaged
  deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''Local
$(deriveJSON defaultOptions ''Local)

data Expose = Exposed | Included | Main FilePath
  deriving (Show, Read, Eq, Ord, Typeable,Data)

deriveSafeCopy 0 'base ''Expose
$(deriveJSON defaultOptions ''Expose)

data PackageMetaData = PackageMetaData
  { pkgMDAuthor :: !Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageMetaData

$(deriveJSON defaultOptions{fieldLabelModifier=Prelude.drop 3} ''PackageMetaData)

instance Default PackageMetaData where
  def = PackageMetaData ""

data Doc = Doc 
  { dShort :: !Text
  , dLong :: !Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)
    
$(deriveJSON defaultOptions{fieldLabelModifier=Prelude.drop 1} ''Doc)
    
instance Default Doc where
  def = Doc "" ""
    
deriveSafeCopy 0 'base ''Doc

newtype ComponentName = ComponentName {unCompName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentName


instance IsString ComponentName where
  fromString = ComponentName . pack

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''ComponentName)

data ComponentType = Library | Executable | Test | BenchMark
    deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentType

$(deriveJSON defaultOptions ''ComponentType)

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


newtype ModuleName = ModuleName {unModName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleName

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''ModuleName)

instance IsString ModuleName where
  fromString = ModuleName . pack

newtype DeclName = DeclName {unDeclName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''DeclName

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''DeclName)

data DeclType = DeclData | DeclNewType | DeclClass | DeclInstance | DeclType 
    | DeclDataFamily | DeclTypeFamily | DeclDataInstance | DeclTypeInstance
    deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''DeclType

$(deriveJSON defaultOptions{unwrapUnaryRecords=True} ''DeclType)

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



data ModuleKey = ModuleKey 
  { modName       :: ModuleName
  , modPackageKey :: PackageKey
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleKey

data ModuleInclusion = ModuleInclusion
  { miComponent :: ComponentName
  , miExpose    :: Expose
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleInclusion

data Module = Module
  { modKey        :: ModuleKey
  , modDoc        :: Doc
  , modComponents :: [ModuleInclusion]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Module



data FullPackage = FullPackage
  { fpPackage :: !Package
  , fpComponents :: ![Component]
  , fpModules :: ![Module]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''FullPackage


instance Indexable Component where
  empty = ixSet
    [ ixFun $ \c -> [ cPackageKey $ cKey c ]
    , ixFun $ \c -> [ cKey c ]
    , ixFun $ \c -> Prelude.map prName $ cRefs c
    ]


instance Indexable Package where
  empty = ixSet
    [ ixFun $ \pkg -> [ pkgKey pkg ]
    , ixFun $ \pkg -> [ pkgName $ pkgKey pkg ]
    , ixFun $ \pkg -> [ pkgLocal $ pkgKey pkg ]
    , ixFun $ \pkg -> [ textToPackageNameCI $ unPkgName $ pkgName $ pkgKey pkg ]
    ]
    
instance Indexable Module where
  empty = ixSet
    [ ixFun $ \mo -> [ modPackageKey $ modKey mo ]
    , ixFun $ \mo -> Prelude.map ((ComponentKey $ modPackageKey $ modKey mo) . miComponent) $ modComponents mo 
    , ixFun $ \mo -> [ modName $ modKey mo ]
    , ixFun $ \mo -> [ modKey mo ]
    ]  

data Database = Database
  { dPackages :: IxSet Package
  , dComponents :: IxSet Component
  , dModules  :: IxSet Module
  } deriving (Data, Typeable)

deriveSafeCopy 0 'base ''Database

instance Default Database where
  def = Database empty empty empty
