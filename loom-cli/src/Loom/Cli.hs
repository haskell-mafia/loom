{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli (
    loom

  , LoomError (..)
  , renderLoomError
  ) where

import           Control.Monad.Trans.Class (lift)

import           Loom.Cli.Asset
import           Loom.Cli.Build
import           Loom.Cli.Manifest
import           Loom.Cli.Process
import           Loom.Cli.Purescript
import           Loom.Cli.Sass

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe)


data LoomError =
    LoomProcessError ProcessError
  | LoomMissingExecutable Text
  | LoomSassError SassError
    deriving (Show)

renderLoomError :: LoomError -> Text
renderLoomError = \case
  LoomProcessError err ->
    renderProcessError err
  LoomMissingExecutable e ->
    "Could not find executable " <> e
  LoomSassError e ->
    renderSassError e

loom :: EitherT LoomError IO ()
loom = do
  join $ loomBuild
    <$> eitherTFromMaybe (LoomMissingExecutable "sass") findSassOnPath
    <*> eitherTFromMaybe (LoomMissingExecutable "purescript") findPurescriptOnPath

loomBuild :: Sass -> Purescript -> EitherT LoomError IO ()
loomBuild sass ps = do
  let
    dist =
      "dist"
    distAssets =
       dist <> "/assets"
    includes =
      SassIncludes "scss/main.scss" [".", "scss"]
  bas <- firstT LoomSassError . withLogging "css" $ buildSass sass includes distAssets
  _psm <- firstT LoomProcessError (withLogging "purs" $ buildPurescript ps)

  -- FIX Currently using the same dist here, but we really should have two separate folders, dev and prod
  bash <- lift $ mapM (hashFile distAssets distAssets) bas
  lift $ writeManifest distAssets (Manifest . join $ [
      maybeToList . fmap ((,) "css" . AssetManifest . pure) $ bash
    ])
  pure ()
