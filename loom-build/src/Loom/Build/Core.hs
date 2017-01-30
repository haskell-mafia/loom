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

import           Loom.Build.Data
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
  deriving (Show)

data LoomResult =
  LoomResult {
      loomResultSass :: [FilePath]
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
    <*> (fmap join . (findFiles (loomConfigRoot config)) . loomConfigComponents) config
    <*> (fmap join . (findFiles (loomConfigRoot config)) . loomConfigSass) config

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved :: LoomBuildConfig -> LoomResolved -> EitherT LoomError IO LoomResult
buildLoomResolved (LoomBuildConfig sass) (LoomResolved output configs) =
  let
    outputName c =
      output </> (T.unpack . renderLoomName . loomConfigResolvedName) c
    input c =
      fmap (loomConfigResolvedRoot c </>)
    buildSass output' inputs =
      Sass.compileSass sass Sass.SassCompressed inputs output'
  in
    LoomResult
      <$> (firstT LoomSassError . fmap join . mapM (\c -> buildSass (outputName c) . input c . loomConfigResolvedSass $ c)) configs

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
