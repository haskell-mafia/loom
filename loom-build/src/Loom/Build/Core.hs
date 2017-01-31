{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Core (
    LoomBuildConfig
  , LoomError (..)
  , LoomResult (..)
  , initialiseBuild
  , buildLoom
  , renderLoomBuildInitisationError
  , renderLoomError
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Loom.Build.Component
import           Loom.Build.Data
import           Loom.Projector (ProjectorError)
import qualified Loom.Projector as Projector
import           Loom.Sass (Sass, SassError)
import qualified Loom.Sass as Sass

import           P

import           System.FilePath ((</>), FilePath)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)

data LoomBuildConfig =
  LoomBuildConfig Sass

data LoomBuildInitialiseError =
    LoomMissingSassExecutable
  deriving (Show)

data LoomError =
    LoomSassError SassError
  | LoomComponentError ComponentError
  | LoomProjectorError ProjectorError
  deriving (Show)

data LoomResult =
  LoomResult {
      loomResultSass :: [FilePath]
    , loomResultProjector :: [FilePath]
    , loomResultComponents :: [Component]
    } deriving (Eq, Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

buildLoom :: LoomBuildConfig -> Loom -> EitherT LoomError IO ()
buildLoom buildConfig loom = do
  resolved <- liftIO $
    LoomResolved (loomOutput loom) <$> mapM resolveLoom (loomConfigs loom)
  void $ buildLoomResolved buildConfig resolved

resolveLoom :: LoomConfig -> IO LoomConfigResolved
resolveLoom config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved :: LoomBuildConfig -> LoomResolved -> EitherT LoomError IO LoomResult
buildLoomResolved (LoomBuildConfig sass) (LoomResolved output configs) = do
  let
    outputName c =
      output </> (T.unpack . renderLoomName . loomConfigResolvedName) c
    input c =
      fmap (loomConfigResolvedRoot c </>)
    buildSass c inputs =
      Sass.compileSass sass Sass.SassCompressed (input c inputs) (outputName c)
    buildProjector c inputs =
      Projector.compileProjector
        (Projector.ModuleName . renderLoomName . loomConfigResolvedName $ c)
        (input c inputs)
        (output </> "src")
    mapJoin f =
      fmap join . mapM f
  components <- fmap join . firstT LoomComponentError . for configs $ \c ->
    fmap (fmap ((,) c)) . resolveComponents . loomConfigResolvedComponents $ c
  LoomResult
    <$> (firstT LoomSassError $
      (<>)
        <$> mapJoin (\c -> buildSass c . loomConfigResolvedSass $ c) configs
        <*> mapJoin (\(c', c) -> buildSass c' . fmap (componentFilePath c) . componentSassFiles $ c) components
      )
    <*> (firstT LoomProjectorError . mapJoin (\(c', c) ->
      buildProjector c' . fmap (componentFilePath c) . componentProjectorFiles $ c
      )) components
    <*> (pure . fmap snd) components

renderLoomBuildInitisationError :: LoomBuildInitialiseError -> Text
renderLoomBuildInitisationError ie =
  case ie of
    LoomMissingSassExecutable ->
      "Could not locate 'sassc' executable on the PATH"

renderLoomError :: LoomError -> Text
renderLoomError le =
  case le of
    LoomSassError se ->
      Sass.renderSassError se
    LoomComponentError e ->
      renderComponentError e
    LoomProjectorError e ->
      Projector.renderProjectorError e
