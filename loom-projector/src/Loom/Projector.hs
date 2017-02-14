{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Projector (
    ProjectorError (..)
  , ProjectorInput (..)
  , ProjectorOutput (..)
  , DataModuleName (..)
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

import qualified Machinator.Core as MC

import           P

import           Projector.Html (DataModuleName (..), ModuleName (..))
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

data ProjectorInput =
  ProjectorInput {
      projectorModuleName :: ModuleName
    , projectorModuleRoot :: FilePath
    , projectorModuleTemplates :: [FilePath]
    } deriving (Show)

data ProjectorOutput =
  ProjectorOutput {
      projectorOutputModules :: [FilePath]
    }

data ProjectorOutputTemp =
  ProjectorOutputTemp {
      projectorOutputTemp :: ProjectorOutput
    -- FIX Expose the correct HtmlModules type
    , _projectorOutputTempBuild :: Projector.BuildArtefacts
    }

instance Monoid ProjectorOutput where
  mempty =
    ProjectorOutput mempty
  mappend (ProjectorOutput d1) (ProjectorOutput d2) =
    ProjectorOutput (d1 <> d2)

compileProjector ::
  [MC.Definition] ->
  [Projector.DataModuleName] ->
  FilePath ->
  [ProjectorInput] ->
  EitherT ProjectorError IO ProjectorOutput
compileProjector udts dns output =
  fmap projectorOutputTemp .
    foldM
      (compileProjectorIncremental udts dns output)
      (ProjectorOutputTemp mempty (Projector.BuildArtefacts mempty mempty))

compileProjectorIncremental ::
  [MC.Definition] ->
  [Projector.DataModuleName] ->
  FilePath ->
  ProjectorOutputTemp ->
  ProjectorInput ->
  EitherT ProjectorError IO ProjectorOutputTemp
compileProjectorIncremental
  udts
  dns
  output
  (ProjectorOutputTemp (ProjectorOutput ot1) (Projector.BuildArtefacts _ oh1))
  (ProjectorInput prefix root inputs) = do
  templates <- for inputs $ \input ->
    fmap ((,) (fromMaybe input . stripPrefix root $ input)) .
      newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $
        input
  Projector.BuildArtefacts ot2 oh2 <- firstT ProjectorError . hoistEither $
    Projector.runBuildIncremental
      (Projector.Build
        -- FIX List of backends to include purescript??
        (Just Projector.Haskell)
        (Projector.moduleNamerSimple (Just . fixModuleName $ prefix))
        dns
        )
      (Projector.UserDataTypes udts)
      oh1
      (Projector.RawTemplates templates)
  ot3 <- liftIO . for ot2 $ \(f', t) -> do
    let
      f = fromMaybe f' . stripPrefix "/" $ f'
    createDirectoryIfMissing True (output </> takeDirectory f)
    T.writeFile (output </> f) t
    pure f
  pure $ ProjectorOutputTemp (ProjectorOutput $ ot1 <> ot3) (Projector.BuildArtefacts mempty (oh1 <> oh2))

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
      "Projector build errors:\n" <> T.unlines (fmap Projector.renderHtmlError es)

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
