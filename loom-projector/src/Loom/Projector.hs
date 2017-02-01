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

import qualified Data.Char as Char
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
    deriving (Eq, Show)

compileProjector :: ModuleName -> [FilePath] -> FilePath -> EitherT ProjectorError IO [FilePath]
compileProjector prefix inputs output = do
  templates <- for inputs $ \input ->
    fmap ((,) input) . newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $ input
  outputs <- firstT ProjectorError . hoistEither $
    Projector.runBuild
      -- FIX List of backends to include purescript??
      (Projector.Build (Just Projector.Haskell) (Projector.ModulePrefix . fixModuleName $ prefix))
      (Projector.RawTemplates templates)
  liftIO . for (Projector.unBuildArtefacts outputs) $ \(f', t) -> do
    let
      f = fromMaybe f' . stripPrefix "/" $ f'
    createDirectoryIfMissing True (output </> takeDirectory f)
    T.writeFile (output </> f) t
    pure f

-- FIX Projector should be doing this for us?
fixModuleName :: ModuleName -> ModuleName
fixModuleName (ModuleName m) =
  ModuleName . T.filter Char.isAlphaNum . T.toTitle $ m

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
