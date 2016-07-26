{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Manifest (
    combineManifests
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import           Loom.Cli.Asset
import           Loom.Cli.Build
import           Loom.Cli.Javascript

import           P

import           System.IO (IO)


combineManifests :: AssetManifest -> [JavascriptManifest] -> IO ()
combineManifests (AssetManifest _ams) js =
  writeToFile "dest/all-manifest.json" $ \out ->
    BSL.writeFile (T.unpack out) . A.encode $ A.object [
        "assets" .= ()
      , "js" .= (A.object . flip fmap js $ \(JavascriptManifest _n _jm) -> "" .= ())
      ]
