{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Fetch.HTTPS.Npm (
    npmFetcher
  ) where


import qualified Data.Text as T

import           Loom.Core.Data
import           Loom.Fetch.Data
import           Loom.Fetch.HTTPS

import           P

import           System.Directory (getTemporaryDirectory)
import           System.IO (IO, FilePath)


npmFetcher :: IO (Fetcher NpmDependency HTTPSError FilePath)
npmFetcher = do
  tmpdir <- getTemporaryDirectory
  httpsFetcher tmpdir npmUri npmRedirectPolicy

npmUri :: NpmDependency -> Uri
npmUri (NpmDependency (NpmPackage pack) (NpmPackageVersion vers) _sha1) =
  Uri . T.unpack $ "https://registry.npmjs.org/" <> pack <> "/-/" <> pack <> "-" <> vers <> ".tgz"

npmRedirectPolicy :: RedirectPolicy
npmRedirectPolicy =
  tlsTldRedirectPolicy "registry.npmjs.org"
