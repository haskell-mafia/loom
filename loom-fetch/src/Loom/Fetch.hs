{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Fetch (
  -- * Types
    FetchedDependency (..)
  -- * IO
  , FetchError (..)
  , renderFetchError
  , fetchDeps
  , fetchDepsSha1
  , fetch
  , fetchSha1
  , validateCachedFile
  , unpackDep
  -- * Caching
  , cacheRoot
  ) where


import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import qualified Control.Concurrent.Async as A
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.Hash.SHA1 as SHA1 (hashlazy)

import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Loom.Core.Data
import           Loom.Fetch.Data

import           P

import           System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import           System.FilePath (FilePath, (</>), (<.>))
import           System.IO (IO, hClose)
import           System.IO.Temp (openTempFile)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left, runEitherT, sequenceEitherT)


data FetchedDependency = FetchedDependency {
    fetchedTarball :: Tarball
  , fetchedSha1 :: Sha1
  } deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

data FetchError e =
    FetchError e
  | FetchException Text
  | Sha1Mismatch FilePath Sha1 Sha1
  | CorruptArchive Text
  deriving (Eq, Ord, Show)

renderFetchError :: (e -> Text) -> FetchError e -> Text
renderFetchError f fe =
  case fe of
    FetchError e ->
      "Fetch error: " <> f e
    FetchException t ->
      "Fetch exception: " <> t
    Sha1Mismatch file want have ->
      "Fetch error: SHA1 mismatch for '" <> T.pack file <> "' (want " <> unSha1 want <> ", have " <> unSha1 have <> ")"
    CorruptArchive t ->
      "Fetch error: could not unpack tarball (" <> t <> ")"

-- | Unpack a fetched tarball to some destination directory.
unpackDep :: FetchedDependency -> FilePath -> EitherT (FetchError e) IO ()
unpackDep (FetchedDependency tar sha) out = do
  liftIO (createDirectoryIfMissing True out)
  _ <- validateCachedFile (tarballFilePath tar) sha
  e <- liftIO $ A.async (unpackTarball out tar) >>= A.waitCatch
  either
    (left . CorruptArchive . T.pack . show)
    pure
    e

-- | Fetch a set of dependencies.
fetchDeps ::
     Traversable t
  => LoomHome
  -> Fetcher a e FilePath
  -> (a -> FilePath)
  -> t a
  -> EitherT [FetchError e] IO (t FetchedDependency)
fetchDeps home fetcher namer deps = do
  liftIO (createDirectoryIfMissing True (cacheRoot home))
  liftIO (createDirectoryIfMissing True (cacheTemp home))
  sequenceEitherT (fmap (firstT pure . fetch home fetcher namer) deps)

-- | Fetch a set of dependencies with expected SHA1 hashes.
fetchDepsSha1 ::
     Traversable t
  => LoomHome
  -> Fetcher a e FilePath
  -> (a -> FilePath)
  -> (a -> Sha1)
  -> t a
  -> EitherT [FetchError e] IO (t FetchedDependency)
fetchDepsSha1 home fetcher namer hashr deps = do
  liftIO (createDirectoryIfMissing True (cacheRoot home))
  liftIO (createDirectoryIfMissing True (cacheTemp home))
  let go a = fetchSha1 home fetcher namer a (hashr a)
  sequenceEitherT (fmap (firstT pure . go) deps)

fetch ::
     LoomHome
  -> Fetcher a e FilePath
  -> (a -> FilePath)
  -> a
  -> EitherT (FetchError e) IO FetchedDependency
fetch home fetcher namer dep = do
  -- Run the fetcher
  let go = runEitherT (runFetcher fetcher dep)
  e <- liftIO $ A.async go >>= A.waitCatch
  tmpfile <- either (left . FetchException . T.pack . show) (hoistEither . first FetchError) e
  lbs <- liftIO (LB.readFile tmpfile)
  -- Hash the result
  let sha1 = sha1sumLB $! lbs
  -- Install into cache. We move it twice:
  -- - once to make "sure" it's on the same partition as the cache
  -- - once to atomically install into the cache
  (tmp, h) <- liftIO (openTempFile (cacheTemp home) "loom-")
  liftIO (LB.hPut h lbs)
  liftIO (hClose h)
  let out = cacheFile home (namer dep) sha1
  liftIO (renameFile tmp out)
  -- TODO Validate filetype? ensure it's a tarball
  pure (FetchedDependency (Tarball out) sha1)

fetchSha1 ::
     LoomHome
  -> Fetcher a e FilePath
  -> (a -> FilePath)
  -> a
  -> Sha1
  -> EitherT (FetchError e) IO FetchedDependency
fetchSha1 home fetcher namer dep sha1 = do
  let out = cacheFile home (namer dep) sha1
  ifM (liftIO $ doesFileExist out)
    (validateCachedFile out sha1)
    (do fd <- fetch home fetcher namer dep
        validateCachedFile (tarballFilePath (fetchedTarball fd)) sha1)

validateCachedFile :: FilePath -> Sha1 -> EitherT (FetchError e) IO FetchedDependency
validateCachedFile out sha1 = do
  have <- liftIO $ fmap (sha1sumLB $!) (LB.readFile out)
  unless (have == sha1) (left (Sha1Mismatch out sha1 have))
  pure (FetchedDependency (Tarball out) sha1)

sha1sumLB :: LB.ByteString -> Sha1
sha1sumLB =
  Sha1 . TE.decodeUtf8 . Hex.encode . SHA1.hashlazy

unpackTarball :: FilePath -> Tarball -> IO ()
unpackTarball fp tar =
  Tar.unpack fp . Tar.read . GZip.decompress =<< LB.readFile (tarballFilePath tar)

-- -----------------------------------------------------------------------------

-- | The path we use to cache dependency tarballs.
cacheRoot :: LoomHome -> FilePath
cacheRoot home =
  loomHomeFilePath home </> "tar"

-- | The path we use for temporary files, should be on the same
-- partition as the 'cacheRoot' or things will get racy.
cacheTemp :: LoomHome -> FilePath
cacheTemp home =
  loomHomeFilePath home </> "tmp"

cacheFile :: LoomHome -> FilePath -> Sha1 -> FilePath
cacheFile home stem sha1 =
  cacheRoot home </> stem <> "-" <> T.unpack (unSha1 sha1) <.> "tar.gz"
