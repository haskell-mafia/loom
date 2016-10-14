{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Manifest (
    Manifest (..)
  , writeManifest
  ) where

import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import           Loom.Cli.Asset
import           Loom.Cli.Build
import           Loom.Cli.File

import           P

import           System.IO (IO)


newtype Manifest =
  Manifest [(Text, AssetManifest)]

writeManifest :: FilePath -> Manifest -> IO ()
writeManifest dist m =
  writeToFile (dist <> "/all-manifest.json") $ \out ->
    BSL.writeFile (T.unpack out) . A.encode $ manifestToJson m

manifestToJson :: Manifest -> Value
manifestToJson (Manifest ams) =
  A.object . flip fmap ams $ \(n, AssetManifest am) ->
    n .= (A.object . flip fmap am $ \hf@(HashedFile f _) -> f .= renderHashedFile hf)
