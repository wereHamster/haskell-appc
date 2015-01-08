{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.AppContainer.Types
   ( ImageManifest(..)
   , ContainerRuntimeManifest(..)

   , App(..)
   , Dependency(..)
   , EventHandler(..)
   , Image(..)
   , Label(..)
   , MountPoint(..)
   , Port(..)
   , Volume(..)
   , VolumeSource(..)
   , HostVolume(..)

   ) where


import           Control.Applicative
import           Control.Monad

import           Data.Text (Text)

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.SemVer
import           Data.UUID
import           Data.Monoid

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
    , imDependencies :: ![Dependency]

    -- , crmAnnotations
    } deriving (Show, Eq)

imageManifestKind :: Text
imageManifestKind = "ImageManifest"



instance FromJSON ImageManifest where
    parseJSON (Object o) = do
        acKind <- o .: "acKind"
        guard $ acKind == imageManifestKind

        ImageManifest
            <$> o .: "name"
            <*> (o .: "acVersion" >>= parseVersion)
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
        , "app"          .= imApp
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

        ContainerRuntimeManifest
            <$> o .: "uuid"
            <*> (o .: "acVersion" >>= parseVersion)
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
    , appEventHandlers :: ![EventHandler]
    , appEnvironment :: !(Map Text Text)
    , appMountPoints :: ![MountPoint]
    , appPorts :: ![Port]
    } deriving (Show, Eq)

instance FromJSON App where
    parseJSON (Object o) = App
        <$> o .: "exec"
        <*> o .: "user"
        <*> o .: "group"
        <*> o .:? "eventHandlers" .!= []
        <*> o .:? "environment" .!= M.empty
        <*> o .:? "mountPoints" .!= []
        <*> o .:? "ports" .!= []

    parseJSON _ = fail "App"

data EventHandler = EventHandler
    { ehName :: !Text
    , ehExec :: ![Text]
    } deriving (Show, Eq)

data MountPoint = MountPoint
    { mpName :: !Text
    , mpPath :: !Text
    , mpReadOnly :: !Bool
    } deriving (Show, Eq)

instance FromJSON MountPoint where
    parseJSON (Object o) = MountPoint
        <$> o .: "name"
        <*> o .: "path"
        <*> o .:? "readOnly" .!= False

    parseJSON _ = fail "MountPoint"

data Volume = Volume
    { volFulfills :: ![Text]
    , volSource :: !VolumeSource
    } deriving (Show, Eq)

instance FromJSON Volume where
    parseJSON v@(Object o) = Volume
        <$> o .: "fulfills"
        <*> parseJSON v

    parseJSON _ = fail "Volume"

instance ToJSON Volume where
    toJSON Volume{..} = case volSource of
        EmptyVolumeSource -> object
            [ "kind"     .= ("empty" :: Text)
            , "fulfills" .= volFulfills
            ]

        HostVolumeSource HostVolume{..} -> object
            [ "kind"     .= ("host" :: Text)
            , "fulfills" .= volFulfills
            , "source"   .= hvSource
            , "readOnly" .= hvReadOnly
            ]


data VolumeSource
    = EmptyVolumeSource
    | HostVolumeSource HostVolume
    deriving (Show, Eq)

instance FromJSON VolumeSource where
    parseJSON v@(Object o) = do
        kind <- o .: "kind"
        case kind :: String of
            "empty" -> return EmptyVolumeSource
            "host"  -> HostVolumeSource <$> parseJSON v
            _       -> fail $ "Unknown volume kind: " <> kind

    parseJSON _ = fail "VolumeSource"


data HostVolume = HostVolume
    { hvSource :: !Text
    , hvReadOnly :: !Bool
    } deriving (Show, Eq)

instance FromJSON HostVolume where
    parseJSON (Object o) = HostVolume
        <$> o .: "source"
        <*> o .:? "readOnly" .!= False

    parseJSON _ = fail "HostVolume"


data Port = Port
    { portName :: !Text
    , portProtocol :: !Text
    , portPort :: !Int
    , portSocketActivated :: !Bool
    } deriving (Show, Eq)

instance FromJSON Port where
    parseJSON (Object o) = Port
        <$> o .: "name"
        <*> o .: "protocol"
        <*> o .: "port"
        <*> o .:? "socketActivated" .!= False

    parseJSON _ = fail "Port"

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


$(deriveToJSON (deriveJSONOptions "app") ''App)
$(deriveJSON (deriveJSONOptions "dep") ''Dependency)
$(deriveJSON (deriveJSONOptions "eh") ''EventHandler)
$(deriveJSON (deriveJSONOptions "label") ''Label)
$(deriveToJSON (deriveJSONOptions "mp") ''MountPoint)
$(deriveToJSON (deriveJSONOptions "port") ''Port)
$(deriveJSON (deriveJSONOptions "image") ''Image)
