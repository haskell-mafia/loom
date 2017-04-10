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
import           Loom.Core.Data
import           Loom.Js (JsError)
import qualified Loom.Js as Js
import           Loom.Machinator (MachinatorInput (..), MachinatorError)
import qualified Loom.Machinator as Machinator
import           Loom.Projector (ProjectorError)
import qualified Loom.Projector as Projector
import           Loom.Purescript (PurescriptError)
import qualified Loom.Purescript as Purescript
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
  | LoomJsError JsError
  | LoomPursError PurescriptError
  deriving (Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

buildLoom ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomHome ->
  LoomTmp ->
  Loom ->
  EitherT LoomError IO LoomResult
buildLoom logger buildConfig home dir (Loom loomConfig' loomConfigs') = do
  resolved <- liftIO $
    LoomResolved
      <$> resolveLoom loomConfig'
      <*> mapM resolveLoom loomConfigs'
  buildLoomResolved logger buildConfig home dir resolved

resolveLoom :: LoomConfig -> IO LoomConfigResolved
resolveLoom config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config
    <*> pure (loomConfigJsDepsNpm config)
    <*> pure (loomConfigJsDepsGithub config)
    <*> pure (loomConfigPursDepsGithub config)

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomHome ->
  LoomTmp ->
  LoomResolved ->
  EitherT LoomError IO LoomResult
buildLoomResolved logger (LoomBuildConfig sass) home dir (LoomResolved config others) = do
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
      outputCss = CssFile $ loomTmpFilePath dir </> (T.unpack . renderLoomName . loomConfigResolvedName) config <> ".css"
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
          (renderLoomName . loomConfigResolvedName $ cr)
          (loomRootFilePath . loomConfigResolvedRoot $ cr)
          images
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

  withLog logger "purs" . firstT LoomPursError $ do
    let
      psdir = Purescript.PurescriptUnpackDir (loomTmpFilePath dir </> "purs")
    deps <- Purescript.fetchPurs home (loomConfigResolvedPursDepsGithub config)
    Purescript.unpackPurs psdir deps

  js <- withLog logger "js" . firstT LoomJsError $ do
    let
      jsdir = Js.JsUnpackDir (loomTmpFilePath dir </> "js")
    -- Fetch and unpack dependencies
    deps <- Js.fetchJs home (loomConfigResolvedJsDepsNpm config) (loomConfigResolvedJsDepsGithub config)
    Js.unpackJs jsdir deps
    pure []

  pure $
    LoomResult
      (loomConfigResolvedName config)
      (Map.fromList (fmap (first loomConfigResolvedName) components))
      mo
      po
      outputCss
      images
      js

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
      T.unlines [
          "Could not locate 'sassc' executable on the PATH."
        , ""
        , "For OSX users try running the following first:"
        , " - brew install sassc"
        , ""
        , "For Arch users try running the following first:"
        , " - pacman -S sassc"
        , ""
        , "Otherwise please follow the build instructions here:"
        , " - https://github.com/sass/sassc#documentation"
        ]

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
    LoomJsError e ->
      Js.renderJsError e
    LoomPursError e ->
      Purescript.renderPurescriptError e

foldMapM :: (Foldable t, Monad m, Monoid b) => (b -> a -> m b) -> t a  -> m b
foldMapM f =
  foldM (\b -> fmap (mappend b) . f b) mempty
