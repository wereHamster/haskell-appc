{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Applicative

import           Test.Hspec
import           Test.SmallCheck
import           Test.SmallCheck.Series
import           Test.Hspec.SmallCheck

import           Data.Monoid         ((<>))
import           Data.Function
import           Data.List
import           Data.Aeson
import           Data.Text (Text)
import           Data.Text as T
import           Data.SemVer
import           Data.UUID

import           GHC.Word

import           Data.AppContainer.Types


instance Monad m => Serial m Word32 where
    series = decDepth $ fromIntegral <$> (series :: Series m Int)

instance Monad m => Serial m UUID where
    series = decDepth $ fromWords
        <$> series
        <*> series
        <*> series
        <*> series

instance Monad m => Serial m Text where
    series = decDepth $ T.pack
        <$> series

instance Monad m => Serial m Version where
    series = decDepth $ version
        <$> fmap getPositive series
        <*> fmap getPositive series
        <*> fmap getPositive series
        <*> pure []
        <*> pure []

instance Monad m => Serial m Label where
    series = decDepth $ Label
        <$> series <*> series

instance Monad m => Serial m ImageManifest where
    series = decDepth $ ImageManifest
        <$> series <*> series <*> series <*> pure Nothing <*> pure []

instance Monad m => Serial m ContainerRuntimeManifest where
    series = decDepth $ ContainerRuntimeManifest
        <$> series
        <*> series
        <*> pure []
        <*> pure []



main :: IO ()
main = do
    hspec spec


spec :: Spec
spec = do

    describe "ImageManifest" $ do
        it "x ≡ fromJSON (toJSON x)" $ property $ \(im :: ImageManifest) ->
            Success im == fromJSON (toJSON im)


    describe "ContainerRuntimeManifest" $ do
        it "x ≡ fromJSON (toJSON x)" $ property $ \(im :: ContainerRuntimeManifest) ->
            Success im == fromJSON (toJSON im)
