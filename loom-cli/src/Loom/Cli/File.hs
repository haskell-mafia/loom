{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.File (
    FilePath
  , FilePattern
  , findFiles
  , takeDirectory
  , dropExtension
  , replaceExtension
  , getDirectoryContents
  , createDirectoryIfMissing
  , getModificationTime
  , renameFile
  , copyFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Text as T
import           Data.Time (UTCTime)

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import qualified System.FilePath.Glob as G
import           System.IO (IO)


-- We're damned if we do and damned if we don't use Text instead of String.
-- If we keep the default FilePath we have to unpack when calling our own functions.
-- If we use Text we have to unpack when calling directly to Text/ByteString IO functions.
type FilePath = Text

type FilePattern = Text

findFiles :: MonadIO m => [FilePattern] -> m [FilePath]
findFiles patterns =
  liftIO $ fmap T.pack . join . fst
    <$> G.globDir (fmap (G.compile . T.unpack) patterns) "."

replaceExtension :: Text -> FilePath -> FilePath
replaceExtension ext =
  (<> "." <> ext) . dropExtension

--
-- Wrappers for the Directory and FilePath functions that are Text and MonadIO friendly
--

dropExtension :: FilePath -> FilePath
dropExtension =
  T.pack . F.dropExtension . T.unpack

takeDirectory :: FilePath -> FilePath
takeDirectory =
  T.pack . F.takeDirectory . T.unpack

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents =
  fmap (filter (not . T.isInfixOf ".") . fmap T.pack) . D.getDirectoryContents . T.unpack

createDirectoryIfMissing :: MonadIO m => Bool -> FilePath -> m ()
createDirectoryIfMissing b =
  liftIO . D.createDirectoryIfMissing b . T.unpack

getModificationTime :: MonadIO m => FilePath -> m UTCTime
getModificationTime =
  liftIO . D.getModificationTime . T.unpack

renameFile :: MonadIO m => FilePath -> FilePath -> m ()
renameFile old new =
  liftIO $ D.renameFile (T.unpack old) (T.unpack new)

copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile old new =
  liftIO $ D.copyFile (T.unpack old) (T.unpack new)
