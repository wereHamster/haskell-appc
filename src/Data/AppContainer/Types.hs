{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.AppContainer.Types
   ( ImageManifest(..)
   , ContainerRuntimeManifest(..)
   , Image(..)
   , App(..)
   , Label(..)
   ) where


import           Control.Applicative
import           Control.Monad

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Map (Map)

import           Data.SemVer
import           Data.UUID

import           Data.Aeson
import           Data.Aeson.Types
import           Data.AppContainer.TH



------------------------------------------------------------------------------
-- ImageManifest

data ImageManifest = ImageManifest
    { imName :: !Text
    , imVersion :: !Version
    , imLabels :: ![Label]
    , imApp :: !(Maybe App)
    , imDependencies :: [Dependency]

    -- , crmAnnotations
    } deriving (Show, Eq)

imageManifestKind :: Text
imageManifestKind = "ImageManifest"



instance FromJSON ImageManifest where
    parseJSON (Object o) = do
        acKind <- o .: "acKind"
        guard $ acKind == imageManifestKind

        versionString <- o .: "acVersion"

        ImageManifest
            <$> o .: "name"
            <*> parseVersion versionString
            <*> o .:? "labels" .!= []
            <*> o .:? "app"
            <*> o .:? "dependencies" .!= []

    parseJSON _ = fail "ImageManifest"

instance ToJSON ImageManifest where
    toJSON ImageManifest{..} = object
        [ "acKind"       .= imageManifestKind
        , "acVersion"    .= toText imVersion
        , "name"         .= imName
        , "labels"       .= imLabels
        , "dependencies" .= imDependencies
        ]



------------------------------------------------------------------------------
-- ContainerRuntimeManifest

data ContainerRuntimeManifest = ContainerRuntimeManifest
    { crmUUID :: !UUID
    , crmVersion :: !Version
    , crmImages :: ![Image]
    , crmVolumes :: ![Volume]

    -- , crmIsolators
    -- , crmAnnotations
    } deriving (Show, Eq)

containerRuntimeManifestKind :: Text
containerRuntimeManifestKind = "ContainerRuntimeManifest"


instance FromJSON ContainerRuntimeManifest where
    parseJSON (Object o) = do
        acKind <- o .: "acKind"
        guard $ acKind == containerRuntimeManifestKind

        versionString <- o .: "acVersion"

        ContainerRuntimeManifest
            <$> o .: "uuid"
            <*> parseVersion versionString
            <*> o .: "apps"
            <*> o .:? "volumes" .!= []

    parseJSON _ = fail "ContainerRuntimeManifest"

instance ToJSON ContainerRuntimeManifest where
    toJSON ContainerRuntimeManifest{..} = object
        [ "acKind"       .= containerRuntimeManifestKind
        , "acVersion"    .= toText crmVersion
        , "uuid"         .= crmUUID
        , "apps"         .= crmImages
        , "volumes"      .= crmVolumes
        ]

data Label = Label
    { labelName :: !Text
    , labelVal :: !Text
    } deriving (Show, Eq)

data Image = Image
    { imageApp :: !Text
    , imageImageID :: !Text

    -- , imageIsolators
    -- , imageAnnotations
    } deriving (Show, Eq)

data App = App
    { appExec :: ![Text]
    , appUser :: !Text
    , appGroup :: !Text
    , appEventHandlers :: !(Maybe [EventHandler])
    , appEnvironment :: !(Maybe (Map Text Text))
    , appMountPoints :: !(Maybe [MountPoint])
    , appPorts :: !(Maybe [Port])
    } deriving (Show, Eq)

data EventHandler = EventHandler
    { ehName :: !Text
    , ehExec :: ![Text]
    } deriving (Show, Eq)

data MountPoint = MountPoint
    { mpName :: !Text
    , mpPath :: !Text
    , mpReadOnly :: !Bool
    } deriving (Show, Eq)

data Volume = Volume
    { volKind :: !Text
    } deriving (Show, Eq)

data Port = Port
    { portName :: !Text
    , portProtocol :: !Text
    , portPort :: !Int
    , portSocketActivated :: !(Maybe Bool)
    } deriving (Show, Eq)

data Dependency = Dependency
    { depName :: !Text
    , depLabels :: ![Label]
    , depHash :: !Text
    , depRoot :: !Text
    } deriving (Show, Eq)


parseVersion :: Text -> Parser Version
parseVersion text = case fromText text of
    Left e  -> fail e
    Right v -> pure v


instance ToJSON UUID where
    toJSON = toJSON . Data.UUID.toString

instance FromJSON UUID where
    parseJSON x = parseJSON x >>= \str -> case Data.UUID.fromString str of
        Nothing   -> fail "UUID"
        Just uuid -> pure uuid


$(deriveJSON (deriveJSONOptions "app") ''App)
$(deriveJSON (deriveJSONOptions "dep") ''Dependency)
$(deriveJSON (deriveJSONOptions "eh") ''EventHandler)
$(deriveJSON (deriveJSONOptions "label") ''Label)
$(deriveJSON (deriveJSONOptions "mp") ''MountPoint)
$(deriveJSON (deriveJSONOptions "vol") ''Volume)
$(deriveJSON (deriveJSONOptions "port") ''Port)
$(deriveJSON (deriveJSONOptions "image") ''Image)
