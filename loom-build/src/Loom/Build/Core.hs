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

import qualified Data.Map as Map
import qualified Data.Text as T

import           Loom.Build.Component
import           Loom.Build.Data
import           Loom.Build.Haskell
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
  | LoomHaskellError LoomHaskellError
  deriving (Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

buildLoom :: LoomBuildConfig -> Loom -> EitherT LoomError IO ()
buildLoom buildConfig (Loom loomOutput' loomConfig' loomConfigs') = do
  resolved <- liftIO $
    LoomResolved loomOutput'
      <$> resolveLoom loomConfig'
      <*> mapM resolveLoom loomConfigs'
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
buildLoomResolved (LoomBuildConfig sass) (LoomResolved output config others) = do
  let
    configs =
      -- Need to make sure the dependencies are in reverse order
      reverse $ config : others
    input c =
      fmap (loomConfigResolvedRoot c </>)
  components <- firstT LoomComponentError . for configs $ \c ->
    fmap ((,) c) . resolveComponents . fmap (loomConfigResolvedRoot c </>) . loomConfigResolvedComponents $ c

  --- SASS ---
  let
    outputCss = Sass.CssFile $ (T.unpack . renderLoomName . loomConfigResolvedName) config <> ".css"
    inputs =
      mconcat . mconcat $ [
          fmap (\c -> input c . loomConfigResolvedSass $ c) configs
        , fmap (\c -> fmap (componentFilePath c) . componentSassFiles $ c) . bind snd $ components
        ]
  firstT LoomSassError $
    Sass.compileSass sass Sass.SassCompressed (Sass.CssFile $ output </> Sass.renderCssFile outputCss) inputs

  --- Machinator ---
  let
    mms = with components $ \(cr, cs) ->
      MachinatorInput
        (Machinator.ModuleName . renderLoomName . loomConfigResolvedName $ cr)
        (loomConfigResolvedRoot cr)
        (bind (\c -> fmap (componentFilePath c) . componentMachinatorFiles $ c) cs)
  mo <- firstT LoomMachinatorError $
    Machinator.compileMachinator mms

  --- Projector ---
  let
    pms = with components $ \(cr, cs) ->
      Projector.ProjectorInput
        (Projector.moduleNameFromFile . T.unpack . renderLoomName . loomConfigResolvedName $ cr)
        (loomConfigResolvedRoot cr)
        (bind (\c -> fmap (componentFilePath c) . componentProjectorFiles $ c) cs)
  po <- firstT LoomProjectorError $
    Projector.compileProjector
      (Map.fromList .
        fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
        Map.toList . Machinator.machinatorOutputDefinitions $ mo
        )
      pms

  --- Images ---
  let
    images = components >>= \(_, cs) ->
      bind (\c -> fmap (ImageFile . componentFilePath c) . componentImageFiles $ c) cs

  let
    result =
      LoomResult
        (loomConfigResolvedName config)
        (bind snd components)
        mo
        po
        outputCss
        images

  firstT LoomHaskellError $ generateHaskell output result

  pure result

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
    LoomHaskellError e ->
      renderLoomHaskellError e
