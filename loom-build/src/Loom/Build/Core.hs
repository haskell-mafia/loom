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

import           Loom.Build.Assets
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

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO (IO)
import qualified System.IO.Temp as Temp

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)

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

buildLoom :: LoomBuildConfig -> LoomSitePrefix -> Loom -> EitherT LoomError IO LoomResult
buildLoom buildConfig spx (Loom loomOutput' loomConfig' loomConfigs') = do
  resolved <- liftIO $
    LoomResolved loomOutput'
      <$> resolveLoom loomConfig'
      <*> mapM resolveLoom loomConfigs'
  result <- buildLoomResolved buildConfig spx resolved
  firstT LoomHaskellError $
    generateHaskell
      (loomResolvedOutput resolved)
      spx
      (loomConfigResolvedAssetsPrefix . loomResolvedConfig $ resolved)
      result
  pure result

resolveLoom :: LoomConfig -> IO LoomConfigResolved
resolveLoom config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (pure . loomConfigAssetsPrefix) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved :: LoomBuildConfig -> LoomSitePrefix -> LoomResolved -> EitherT LoomError IO LoomResult
buildLoomResolved (LoomBuildConfig sass) spx (LoomResolved output config others) = do
  let
    configs =
      -- Need to make sure the dependencies are in reverse order
      reverse $ config : others
  components <- firstT LoomComponentError . for configs $ \c ->
    fmap ((,) c) . resolveComponents . loomConfigResolvedComponents $ c

  liftIO $ createDirectoryIfMissing True output

  --- Images ---
  let
    images = components >>= \(cr, cs) ->
      bind (\c -> fmap (ImageFile (loomConfigResolvedName cr)) . componentImageFiles $ c) cs

  --- SASS ---
  let
    outputCss = Sass.CssFile $ (T.unpack . renderLoomName . loomConfigResolvedName) config <> ".css"
    inputs =
      mconcat . mconcat $ [
          fmap (\c -> fmap loomFilePath . loomConfigResolvedSass $ c) configs
        , fmap (fmap componentFilePath . componentSassFiles) . bind snd $ components
        ]
  newEitherT . Temp.withTempDirectory output "loom.css" $ \dir ->
    runEitherT $ do
      let
        outputCssTemp = Sass.CssFile $ dir </> "loom-tmp.css"
        outputCssAbs = Sass.CssFile $ output </> Sass.renderCssFile outputCss
      firstT LoomSassError $
        Sass.compileSass sass Sass.SassCompressed outputCssTemp inputs
      liftIO $
        prefixCssImageAssets spx (loomConfigResolvedAssetsPrefix config) images outputCssAbs outputCssTemp

  --- Machinator ---
  let
    mms = with components $ \(cr, cs) ->
      MachinatorInput
        (Machinator.ModuleName . renderLoomName . loomConfigResolvedName $ cr)
        (loomRootFilePath . loomConfigResolvedRoot $ cr)
        (bind (fmap componentFilePath . componentMachinatorFiles) cs)
  mo <- firstT LoomMachinatorError $
    Machinator.compileMachinator mms

  --- Projector ---
  let
    pms = with components $ \(cr, cs) ->
      Projector.ProjectorInput
        (Projector.moduleNameFromFile . T.unpack . renderLoomName . loomConfigResolvedName $ cr)
        (loomRootFilePath . loomConfigResolvedRoot $ cr)
        (bind (fmap componentFilePath . componentProjectorFiles) cs)
  po <- firstT LoomProjectorError $
    Projector.compileProjector
      (Map.fromList .
        fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
        Map.toList . Machinator.machinatorOutputDefinitions $ mo
        )
      pms

  pure $
    LoomResult
      output
      (loomConfigResolvedName config)
      (bind snd components)
      mo
      po
      outputCss
      images

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
