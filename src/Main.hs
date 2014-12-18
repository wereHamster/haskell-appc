{-# LANGUAGE RecordWildCards #-}

module Main where


import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.SemVer
import Data.Monoid
import Options.Applicative
import System.Exit

import Data.AppContainer
import Data.AppContainer.Types



main :: IO ()
main = run =<< execParser
    (parseOptions `withInfo` "AppContainer tool")


run :: Options -> IO ()
run (Options (VerifyImageManifest path)) = do
    ok <- verifyImageManifest path
    if ok
        then exitSuccess
        else exitFailure


run (Options (VerifyContainerRuntimeManifest path)) = do
    ok <- verifyContainerRuntimeManifest path
    if ok
        then exitSuccess
        else exitFailure


run (Options (BuildImage path output)) = do
    buildImage path output


data Command
    = VerifyImageManifest !String
    | VerifyContainerRuntimeManifest !String
    | BuildImage !String !String

data Options = Options !Command

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [ command "verify-image-manifest"
        (parseVerifyImageManifest `withInfo` "Verify an image manifest")
    , command "verify-container-runtime-manifest"
        (parseVerifyContainerRuntimeManifest `withInfo` "Verify a container runtime manifest")
    , command "build-image"
        (parseBuildImage `withInfo` "Build an image")
    ]

parseVerifyImageManifest :: Parser Command
parseVerifyImageManifest = VerifyImageManifest
    <$> argument str (metavar "PATH")

parseVerifyContainerRuntimeManifest :: Parser Command
parseVerifyContainerRuntimeManifest = VerifyContainerRuntimeManifest
    <$> argument str (metavar "PATH")

parseBuildImage :: Parser Command
parseBuildImage = BuildImage
    <$> argument str (metavar "PATH")
    <*> argument str (metavar "OUTPUT")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
