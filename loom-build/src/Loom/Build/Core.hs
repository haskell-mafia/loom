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
  , machinatorOutputToProjector
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           Loom.Build.Component
import           Loom.Build.Data
import           Loom.Build.Logger
import           Loom.Projector (ProjectorError)
import qualified Loom.Projector as Projector
import           Loom.Machinator (MachinatorInput (..), MachinatorError)
import qualified Loom.Machinator as Machinator
import           Loom.Sass (Sass, SassError)
import qualified Loom.Sass as Sass

import           P

import           System.FilePath ((</>))
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
  | LoomMachinatorError MachinatorError
  deriving (Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

buildLoom ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomTmp ->
  Loom ->
  EitherT LoomError IO LoomResult
buildLoom logger buildConfig dir (Loom loomOutput' loomConfig' loomConfigs') = do
  resolved <- liftIO $
    LoomResolved loomOutput'
      <$> resolveLoom loomConfig'
      <*> mapM resolveLoom loomConfigs'
  buildLoomResolved logger buildConfig dir resolved

resolveLoom :: LoomConfig -> IO LoomConfigResolved
resolveLoom config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (pure . loomConfigAssetsPrefix) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomTmp ->
  LoomResolved ->
  EitherT LoomError IO LoomResult
buildLoomResolved logger (LoomBuildConfig sass) dir (LoomResolved _output config others) = do
  let
    configs =
      -- Need to make sure the dependencies are in reverse order
      reverse $ config : others
  components <- firstT LoomComponentError . for configs $ \c ->
    fmap ((,) c) . resolveComponents . loomConfigResolvedComponents $ c

  --- Images ---
  let
    images = components >>= \(cr, cs) ->
      bind (\c -> fmap (ImageFile (loomConfigResolvedName cr)) . componentImageFiles $ c) cs

  outputCss <- withLog logger "sass" $ do
    let
      outputCss = Sass.CssFile $ loomTmpFilePath dir </> (T.unpack . renderLoomName . loomConfigResolvedName) config <> ".css"
      inputs =
        mconcat . mconcat $ [
            fmap (\c -> fmap loomFilePath . loomConfigResolvedSass $ c) configs
          , fmap (fmap componentFilePath . componentSassFiles) . bind snd $ components
          ]
    firstT LoomSassError $
      Sass.compileSass sass Sass.SassCompressed outputCss inputs
    pure outputCss

  mo <- withLog logger "machinator" $ do
    let
      mms = with components $ \(cr, cs) ->
        MachinatorInput
          (Machinator.ModuleName . renderLoomName . loomConfigResolvedName $ cr)
          (loomRootFilePath . loomConfigResolvedRoot $ cr)
          (bind (fmap componentFilePath . componentMachinatorFiles) cs)
    firstT LoomMachinatorError $
      Machinator.compileMachinator mms

  po <- withLog logger "projector" $ do
    let
      pms = with components $ \(cr, cs) ->
        Projector.ProjectorInput
          (Projector.moduleNameFromFile . T.unpack . renderLoomName . loomConfigResolvedName $ cr)
          (loomRootFilePath . loomConfigResolvedRoot $ cr)
          (bind (fmap componentFilePath . componentProjectorFiles) cs)
    firstT LoomProjectorError $
      foldMapM
        (Projector.compileProjector
          (Map.fromList .
            fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
            Map.toList . Machinator.machinatorOutputDefinitions $ mo
            )
          )
        pms

  pure $
    LoomResult
      (loomConfigResolvedName config)
      (bind snd components)
      mo
      po
      outputCss
      images

machinatorOutputToProjector ::
  Machinator.MachinatorOutput ->
  Map Projector.DataModuleName [Machinator.Definition]
machinatorOutputToProjector =
  Map.fromList .
    fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
    Map.toList . Machinator.machinatorOutputDefinitions

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
    LoomMachinatorError e ->
      Machinator.renderMachinatorError e

foldMapM :: (Foldable t, Monad m, Monoid b) => (b -> a -> m b) -> t a  -> m b
foldMapM f =
  foldM (\b -> fmap (mappend b) . f b) mempty
