{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Projector (
    ProjectorError (..)
  , ModuleName (..)
  , compileProjector
  , renderProjectorError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (handleIf)

import           Data.List (stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           P

import           Projector.Html (ModuleName (..))
import qualified Projector.Html as Projector

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (FilePath, (</>), takeDirectory)
import           System.IO (IO)
import           System.IO.Error (isDoesNotExistError)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, hoistEither)

data ProjectorError =
    ProjectorFileMissing FilePath
  | ProjectorError [Projector.HtmlError]

compileProjector :: ModuleName -> [FilePath] -> FilePath -> EitherT ProjectorError IO [FilePath]
compileProjector prefix inputs output = do
  templates <- for inputs $ \input ->
    fmap ((,) input) . newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $ input
  outputs <- firstT ProjectorError . hoistEither $
    Projector.runBuild
      -- FIX List of backends to include purescript??
      (Projector.Build (Just Projector.Haskell) (Projector.ModulePrefix prefix))
      (Projector.RawTemplates templates)
  liftIO . for (Projector.unBuildArtefacts outputs) $ \(f', t) -> do
    let
      f = fromMaybe f' . stripPrefix "/" $ f'
    createDirectoryIfMissing True (output </> takeDirectory f)
    T.writeFile (output </> f) t
    pure f

renderProjectorError :: ProjectorError -> Text
renderProjectorError pe =
  case pe of
    ProjectorFileMissing f ->
      "Could not find file: " <> T.pack f
    ProjectorError es ->
      "Build errors:\n" <> T.unlines (fmap Projector.renderHtmlError es)

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
