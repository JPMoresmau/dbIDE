{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module Language.Haskell.ASBrowser.Types where

import Language.Haskell.ASBrowser.Utils

import Control.Arrow
import Control.Monad
import Data.Data
import Data.SafeCopy hiding (Version)
import Data.Text hiding (empty,map,drop)
import Distribution.Version
import Distribution.Text (simpleParse, display)
import Data.IxSet.Typed
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

-- deriveJSON defaultOptions{unwrapUnaryRecords=True} ''PackageName
instance ToJSON PackageName where
    toJSON = toJSON . unPkgName
instance FromJSON PackageName where
    parseJSON (String s) = pure $ PackageName s
    parseJSON _          = mzero

instance IsString PackageName where
  fromString = PackageName . pack

newtype PackageNameCI = PackageNameCI {unPkgNameCI :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

--deriveJSON defaultOptions{unwrapUnaryRecords=True} ''PackageNameCI
instance ToJSON PackageNameCI where
    toJSON  = toJSON . unPkgNameCI
instance FromJSON PackageNameCI where
    parseJSON (String s) = pure $ PackageNameCI s
    parseJSON _          = mzero



textToPackageNameCI :: Text -> PackageNameCI
textToPackageNameCI = PackageNameCI . Data.Text.toLower

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
deriveJSON defaultOptions ''Local

data Expose = Exposed | Included | Main FilePath
  deriving (Show, Read, Eq, Ord, Typeable,Data)

deriveSafeCopy 0 'base ''Expose
deriveJSON defaultOptions ''Expose

data PackageMetaData = PackageMetaData
  { pkgMDAuthor :: !Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageMetaData

deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''PackageMetaData

instance Default PackageMetaData where
  def = PackageMetaData ""

data Doc = Doc
  { dShort :: !Text
  , dLong :: !Text
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''Doc

instance Default Doc where
  def = Doc "" ""

deriveSafeCopy 0 'base ''Doc

newtype ComponentName = ComponentName {unCompName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentName


instance IsString ComponentName where
  fromString = ComponentName . pack

--deriveJSON defaultOptions{unwrapUnaryRecords=True} ''ComponentName
instance ToJSON ComponentName where
    toJSON = toJSON . unCompName
instance FromJSON ComponentName where
    parseJSON (String s) = pure $ ComponentName s
    parseJSON _          = mzero

data ComponentType = Library | Executable | Test | BenchMark
    deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''ComponentType

deriveJSON defaultOptions ''ComponentType

newtype URL = URL {unURLName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''URL

instance IsString URL where
  fromString = URL . pack

--deriveJSON defaultOptions{unwrapUnaryRecords=True} ''URL
instance ToJSON URL where
    toJSON = toJSON . unURLName
instance FromJSON URL where
    parseJSON (String s) = pure $ URL s
    parseJSON _          = mzero

data URLs = URLs
  { uSrcURL :: !(Maybe URL)
  , uDocURL :: !(Maybe URL)
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''URLs

deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''URLs

instance Default URLs where
  def = URLs Nothing Nothing

data PackageKey = PackageKey
  { pkgName       :: !PackageName
  , pkgVersion    :: !Version
  , pkgLocal      :: !Local
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''PackageKey
deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''PackageKey

data PackageRef = PackageRef
  { prName :: !PackageName
  , prRange :: !VersionRange
  } deriving (Show,Read,Eq,Typeable,Data)

deriveSafeCopy 0 'base ''PackageRef
deriveJSON defaultOptions{fieldLabelModifier=jsonField 2} ''PackageRef

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
deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''ComponentKey

data Component = Component
 { cKey        :: !ComponentKey
 , cType       :: !ComponentType
 , cRefs       :: [PackageRef]
 , cExtensions :: [Text] -- don't use the Extension type since it only got Ord instance recently, etc
 } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Component
deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''Component

newtype ModuleName = ModuleName {unModName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleName
--deriveJSON defaultOptions{unwrapUnaryRecords=True} ''ModuleName
instance ToJSON ModuleName where
    toJSON = toJSON . unModName
instance FromJSON ModuleName where
    parseJSON (String s) = pure $ ModuleName s
    parseJSON _          = mzero

instance IsString ModuleName where
  fromString = ModuleName . pack


data Location = Location
  { lLine   :: !Int
  , lColumn :: !Int
  } deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''Location
deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''Location

data MarkerLevel = MLError | MLWarning | MLInfo
  deriving (Show,Read,Eq,Ord,Typeable,Data,Bounded,Enum)

deriveSafeCopy 0 'base ''MarkerLevel
deriveJSON defaultOptions ''MarkerLevel

data Marker = Marker
  { mText :: Text
  , mLevel :: MarkerLevel
  , mLocation :: Location
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Marker
deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''Marker


newtype DeclName = DeclName {unDeclName :: Text}
  deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''DeclName
--deriveJSON defaultOptions{unwrapUnaryRecords=True} ''DeclName
instance ToJSON DeclName where
    toJSON = toJSON . unDeclName
instance FromJSON DeclName where
    parseJSON (String s) = pure $ DeclName s
    parseJSON _          = mzero

instance IsString DeclName where
  fromString = DeclName . pack

data DeclType = DeclData | DeclNewType | DeclClass | DeclInstance | DeclType
    | DeclDataFamily | DeclTypeFamily | DeclDataInstance | DeclTypeInstance
    | DeclFunction | DeclConstructor | DeclMethod
    deriving (Show, Read, Eq, Ord, Bounded,Enum,Typeable,Data)

deriveSafeCopy 0 'base ''DeclType

deriveJSON defaultOptions ''DeclType


data WriteDate = WriteDate
  { wdCreated :: UTCTime
  , wdUpdated :: UTCTime
  } deriving (Show, Read, Eq, Ord, Typeable,Data)


deriveSafeCopy 0 'base ''WriteDate

data Package = Package
  { pkgKey        :: !PackageKey
  , pkgDoc        :: !Doc
  , pkgMeta       :: !PackageMetaData
  , pkgDocURL     :: !(Maybe URL)
  , pkgModulesAnalysedDate :: !(Maybe UTCTime)
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Package
deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''Package


data ModuleKey = ModuleKey
  { modName       :: ModuleName
  , modPackageKey :: PackageKey
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleKey
deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''ModuleKey

data ModuleInclusion = ModuleInclusion
  { miComponent :: ComponentName
  , miExpose    :: Expose
  , miMarkers   :: Maybe [Marker]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''ModuleInclusion
deriveJSON defaultOptions{fieldLabelModifier=jsonField 2} ''ModuleInclusion

data Module = Module
  { modKey        :: ModuleKey
  , modDoc        :: Doc
  , modComponents :: [ModuleInclusion]
  , modURLs       :: URLs
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Module
deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''Module

data DeclKey = DeclKey
    { decName   :: DeclName
    , decModule :: ModuleKey
    } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''DeclKey
deriveJSON defaultOptions{fieldLabelModifier=jsonField 3} ''DeclKey

data Decl = Decl
  { dKey :: DeclKey
  , dType :: DeclType
  , dSignature :: Text
  , dDoc  :: Doc
  } deriving (Show,Read,Eq,Ord,Typeable,Data)

deriveSafeCopy 0 'base ''Decl
deriveJSON defaultOptions{fieldLabelModifier=jsonField 1} ''Decl



data FullPackage = FullPackage
  { fpPackage :: !Package
  , fpComponents :: ![Component]
  , fpModules :: ![Module]
  } deriving (Show,Read,Eq,Ord,Typeable,Data)


deriveSafeCopy 0 'base ''FullPackage
deriveJSON defaultOptions{fieldLabelModifier=jsonField 2} ''FullPackage

type ComponentIxs = '[PackageKey, ComponentKey,PackageName]
type IxComponent  = IxSet ComponentIxs Component

instance Indexable ComponentIxs Component where
  indices = ixList
    (ixFun $ \c -> [ cPackageKey $ cKey c ])
    (ixFun $ \c -> [ cKey c ])
    (ixFun $ \c -> Prelude.map prName $ cRefs c)


type PackageIxs = '[PackageKey, PackageName, Local, PackageNameCI]
type IxPackage  = IxSet PackageIxs Package

instance Indexable PackageIxs Package where
  indices = ixList
    (ixFun $ \pkg -> [ pkgKey pkg ])
    (ixFun $ \pkg -> [ pkgName $ pkgKey pkg ])
    (ixFun $ \pkg -> [ pkgLocal $ pkgKey pkg ])
    (ixFun $ \pkg -> [ textToPackageNameCI $ unPkgName $ pkgName $ pkgKey pkg ])


type ModuleIxs = '[PackageKey, ComponentKey, ModuleName, ModuleKey]
type IxModule  = IxSet ModuleIxs Module

instance Indexable ModuleIxs Module where
  indices = ixList
    (ixFun $ \mo -> [ modPackageKey $ modKey mo ])
    (ixFun $ \mo -> Prelude.map ((ComponentKey $ modPackageKey $ modKey mo) . miComponent) $ modComponents mo)
    (ixFun $ \mo -> [ modName $ modKey mo ])
    (ixFun $ \mo -> [ modKey mo ])

type DeclIxs = '[PackageKey, ModuleKey , DeclName]
type IxDecl = IxSet DeclIxs Decl

instance Indexable DeclIxs Decl where
    indices = ixList
        (ixFun $ \de -> [modPackageKey $ decModule $ dKey de])
        (ixFun $ \de -> [decModule $ dKey de])
        (ixFun $ \de -> [decName $ dKey de])

data Database = Database
  { dPackages :: IxPackage
  , dComponents :: IxComponent
  , dModules  :: IxModule
  } deriving (Typeable)

deriveSafeCopy 0 'base ''Database

instance Default Database where
  def = Database empty empty empty
