{-# LANGUAGE RecordWildCards #-}

module Data.AppContainer
    ( verifyImageManifest
    , verifyContainerRuntimeManifest
    , buildImage
    ) where


import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.AppContainer.Types



verifyImageManifest :: String -> IO Bool
verifyImageManifest path = do
    c <- LBS.readFile path
    return $ case eitherDecode c of
        Left _ -> False
        Right ImageManifest{..} -> True


verifyContainerRuntimeManifest :: String -> IO Bool
verifyContainerRuntimeManifest path = do
    c <- LBS.readFile path
    return $ case eitherDecode c of
        Left _ -> False
        Right ContainerRuntimeManifest{..} -> True


buildImage :: String -> String -> IO ()
buildImage path output = do
    return ()
