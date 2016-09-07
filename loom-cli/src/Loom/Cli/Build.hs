{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Loom.Cli.Build (
    Hash
  , UnmodifiedFiles (..)
  , ModifiedFiles (..)
  , writeToFile
  , withLogging
  , hashFiles
  , modules
  , takeDirectory
  , dropExtension
  , replaceExtension
  , getDirectoryContents
  , findFilesWithStatus
  ) where

import           Control.Monad.Catch (tryJust)
import           Control.Monad.IO.Class (MonadIO (..))

import           Crypto.Hash (Digest, MD5, hashlazy)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Time (UTCTime (..), Day (..), diffUTCTime, getCurrentTime)

import           Loom.Cli.File

import           P

import           System.IO (IO, putStrLn)
import           System.IO.Error (isDoesNotExistError)


type Hash = Digest MD5

-- | Ensure that the file being written to has a parent directory
-- In future this may also create a temporary file to be used during the write
writeToFile :: MonadIO m => FilePath -> (FilePath -> m a) -> m a
writeToFile out f = do
  createDirectoryIfMissing True (takeDirectory out)
  -- FIX Maybe do an atomic move here of a temporary file?
  f out

withLogging :: MonadIO m => [Char] -> m a -> m a
withLogging out ma = do
  before <- liftIO $ do
    before <- getCurrentTime
    putStrLn $ "Starting '" <> out <> "'"
    pure before
  a <- ma
  liftIO $ do
    after <- getCurrentTime
    liftIO . putStrLn $ "Finished '" <> out <> "' after " <> (show $ diffUTCTime after before)
  return a

-- Static asset revisioning by appending content hash to filenames unicorn.css -> unicorn-d41d8cd98f.css
--
-- Modelled on the gulp library:
--
-- https://github.com/sindresorhus/gulp-rev
hashFiles :: FilePath -> [FilePath] -> IO [(FilePath, Hash)]
hashFiles dir fs =
  for fs $ \f -> do
    h <- hashlazy <$> BSL.readFile (T.unpack f)
    let nf = dir <> "-" <> (T.pack . show) h
    copyFile f nf
    pure (nf, h)

-- FIX This should be handled as configuration at the top-level, not here
modules :: [Text] -> [FilePattern]
modules exts =
  exts >>= \ext ->
    ["modules/**/*." <> ext, "components/**/*." <> ext]

-----------------------
-- FIX Currently unused
-- These functions are still in development
-- For now everything will rebuild all the time, which at least is reliable
-----------------------

newtype UnmodifiedFiles =
  UnmodifiedFiles [FilePath]
  deriving (Eq, Show)

newtype ModifiedFiles =
  ModifiedFiles [FilePath]
  deriving (Eq, Show)

findFilesWithStatus :: MonadIO m => FilePath -> [FilePattern] -> m (UnmodifiedFiles, ModifiedFiles)
findFilesWithStatus out patterns = do
  files <- findFiles patterns
  m <- liftIO . fmap (either (const $ UTCTime (ModifiedJulianDay 0) 0) id) .
    tryJust (\e -> valueOrEmpty (isDoesNotExistError e) ()) $ getModificationTime out
  fmap (bimap UnmodifiedFiles ModifiedFiles . partitionEithers) . forM files $ \f -> do
    m2 <- getModificationTime f
    pure $ if m < m2 then Right f else Left f

