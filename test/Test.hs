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

import           Data.AppContainer.Types


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



main :: IO ()
main = do
    hspec spec


spec :: Spec
spec = do

    -- The roundtrips test whether the driver generates the proper terms
    -- and the server responds with what the driver expects.
    describe "ImageManifest" $ do
        it "x ≡ fromJSON (toJSON x)" $ property $ \(im :: ImageManifest) ->
            monadic $ do
                res <- return $ fromJSON $ toJSON im
                return $ res == Success im
