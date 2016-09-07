{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli (
    loom

  , LoomError (..)
  , renderLoomError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Loom.Cli.Asset
import           Loom.Cli.Build
import           Loom.Cli.Handlebars
import           Loom.Cli.Javascript
import           Loom.Cli.Manifest
import           Loom.Cli.Process
import           Loom.Cli.Purescript
import           Loom.Cli.Sass

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data LoomError =
    LoomProcessError ProcessError
  | LoomSassError SassError
    deriving (Show)

renderLoomError :: LoomError -> Text
renderLoomError = \case
  LoomProcessError err ->
    renderProcessError err
  LoomSassError err ->
    renderSassError err

loom :: EitherT LoomError IO ()
loom = do
  loomBuild
    (Sass "sassc")
    (Purescript
      (Psc "psc")
      (PscBundle "psc-bundle")
      )
    (BrowserifyInc "node_modules/.bin/browserifyinc")

loomBuild :: Sass -> Purescript -> BrowserifyInc -> EitherT LoomError IO ()
loomBuild sass ps bi = do
  let
    includes =
      SassIncludes [".", "scss"]

    bassets = do
      am <- firstT LoomProcessError . withLogging "assets" $ liftIO buildAssets
      firstT LoomSassError . withLogging "css" $ buildSass sass includes am

  -- FIX Run in Parallel
  (am, _tl, psm) <- (,,)
    <$> bassets
    <*> firstT LoomProcessError (withLogging "hbs" $ liftIO buildTemplates)
    <*> firstT LoomProcessError (withLogging "purs" $ buildPurescript ps)
  js <- firstT LoomProcessError . withLogging "js" $ buildJavascript bi
    (maybeToList . fmap purescriptRequire $ psm)
  liftIO $ combineManifests am js
