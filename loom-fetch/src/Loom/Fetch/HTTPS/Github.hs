{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Fetch.HTTPS.Github (
    githubFetcher
  ) where


import qualified Data.Text as T

import           Loom.Core.Data
import           Loom.Fetch.Data
import           Loom.Fetch.HTTPS

import           P

import           System.Directory (getTemporaryDirectory)
import           System.IO (IO, FilePath)


githubFetcher :: IO (Fetcher GithubDependency HTTPSError FilePath)
githubFetcher = do
  tmpdir <- getTemporaryDirectory
  httpsFetcher tmpdir githubUri githubRedirectPolicy

githubUri :: GithubDependency -> Uri
githubUri (GithubDependency (GithubRepo user repo) (GitRef ref) _sha1) =
  Uri . T.unpack $ "https://github.com/" <> user <> "/" <> repo <> "/tarball/" <> ref

githubRedirectPolicy :: RedirectPolicy
githubRedirectPolicy =
  tlsTldRedirectPolicy "github.com"
