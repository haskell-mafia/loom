{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Fetch.HTTPS.Github (
    githubFetcher
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import           Loom.Core.Data
import           Loom.Fetch.Data
import           Loom.Fetch.HTTPS

import           P

import           System.Directory (getTemporaryDirectory, getDirectoryContents, renameDirectory)
import           System.FilePath ((</>), takeFileName)
import           System.IO (IO, FilePath)
import           System.IO.Temp (withTempDirectory)


githubFetcher :: IO (Fetcher GithubDependency HTTPSError FilePath)
githubFetcher = do
  tmpdir <- getTemporaryDirectory
  fetcher <- httpsFetcher tmpdir githubUri githubRetryPolicy githubRedirectPolicy
  pure . Fetcher $ \dep -> do
    case ghdType dep of
      GithubDependencyV1 ->
        runFetcher fetcher dep
      GithubDependencyV2 -> do
        out <- runFetcher fetcher dep
        liftIO . withTempDirectory tmpdir "loom-github" $ \tmpdir' -> do
          let
            -- The arbitrary name "package" here is relevant to the sha1
            -- It shouldn't be changed without introducing a new github type version
            -- FIX Just move everything down a directoy, but our unpack logic needs
            -- to change in a few places first.
            name = "package"
            -- Reuse the random directory name for our returned file
            outNew = tmpdir </> takeFileName tmpdir' <> ".tar.gz"
          Tar.unpack tmpdir' . Tar.read . GZip.decompress =<< LB.readFile out
          base <- fmap (filter (not . flip elem [".", ".."])) $
            getDirectoryContents tmpdir'
          case base of
            [base'] -> do
              renameDirectory (tmpdir' </> base') (tmpdir' </> name)
              LB.writeFile outNew . GZip.compress . Tar.write =<< Tar.pack tmpdir' [name]
              pure outNew
            _ ->
              pure out

githubUri :: GithubDependency -> Uri
githubUri (GithubDependency (GithubRepo user repo) (GitRef ref) _sha1 _) =
  Uri . T.unpack $ "https://github.com/" <> user <> "/" <> repo <> "/tarball/" <> ref

githubRedirectPolicy :: RedirectPolicy
githubRedirectPolicy =
  tlsTldRedirectPolicy "github.com"

githubRetryPolicy :: RetryPolicy
githubRetryPolicy =
  ExponentialBackoffX 3
