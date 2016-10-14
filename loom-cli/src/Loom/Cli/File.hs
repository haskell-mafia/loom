{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.File (
    FilePath
  , FilePattern
  , findFiles
  , findFilesIn
  , readUtf8
  , writeUtf8
  , readBytes
  , writeBytes
  , takeDirectory
  , takeFileName
  , dropExtension
  , takeExtension
  , replaceExtension
  , getDirectoryContents
  , createDirectoryIfMissing
  , doesFileExist
  , getModificationTime
  , renameFile
  , copyFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
findFiles =
  findFilesIn "."

findFilesIn :: MonadIO m => FilePath -> [FilePattern] -> m [FilePath]
findFilesIn dir patterns =
  liftIO $ fmap T.pack . join . fst
    <$> G.globDir (fmap (G.compile . T.unpack) patterns) (T.unpack dir)

replaceExtension :: Text -> FilePath -> FilePath
replaceExtension ext =
  (<> "." <> ext) . dropExtension

readUtf8 :: MonadIO m => FilePath -> m (Maybe Text)
readUtf8 path =
  fmap T.decodeUtf8 <$> readBytes path

writeUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeUtf8 path text =
  writeBytes path (T.encodeUtf8 text)

readBytes :: MonadIO m => FilePath -> m (Maybe ByteString)
readBytes path =
  liftIO $ do
    exists <- doesFileExist path
    case exists of
      False ->
        pure Nothing
      True  ->
        fmap Just . B.readFile $ T.unpack path

writeBytes :: MonadIO m => FilePath -> ByteString -> m ()
writeBytes path bytes =
  liftIO $ B.writeFile (T.unpack path) bytes

--
-- Wrappers for the Directory and FilePath functions that are Text and MonadIO friendly
--

dropExtension :: FilePath -> FilePath
dropExtension =
  T.pack . F.dropExtension . T.unpack

takeExtension :: FilePath -> FilePath
takeExtension =
  T.pack . F.takeExtension . T.unpack

takeDirectory :: FilePath -> FilePath
takeDirectory =
  T.pack . F.takeDirectory . T.unpack

takeFileName :: FilePath -> FilePath
takeFileName =
  T.pack . F.takeFileName . T.unpack

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents =
  fmap (filter (not . T.isInfixOf ".") . fmap T.pack) . D.getDirectoryContents . T.unpack

createDirectoryIfMissing :: MonadIO m => Bool -> FilePath -> m ()
createDirectoryIfMissing b =
  liftIO . D.createDirectoryIfMissing b . T.unpack

doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist =
  liftIO . D.doesFileExist . T.unpack

getModificationTime :: MonadIO m => FilePath -> m UTCTime
getModificationTime =
  liftIO . D.getModificationTime . T.unpack

renameFile :: MonadIO m => FilePath -> FilePath -> m ()
renameFile old new =
  liftIO $ D.renameFile (T.unpack old) (T.unpack new)

copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile old new =
  liftIO $ D.copyFile (T.unpack old) (T.unpack new)
