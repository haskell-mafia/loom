{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli (
    loom
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


loom :: EitherT ProcessError IO ()
loom = do
  loomBuild
    (Sass "sass")
    (Purescript
      (Psc "psc")
      (PscBundle "psc-bundle")
      )
    (BrowserifyInc "node_modules/.bin/browserifyinc")

loomBuild :: Sass -> Purescript -> BrowserifyInc -> EitherT ProcessError IO ()
loomBuild sass ps bi = do
  -- FIX Run in Parallel
  (am, _tl, psm) <- (,,)
    <$> (withLogging "assets" (liftIO buildAssets) >>= withLogging "css" . buildSass sass)
    <*> withLogging "hbs" (liftIO buildTemplates)
    <*> (withLogging "purs" $ buildPurescript ps)
  js <- withLogging "js" $ buildJavascript bi
    (maybeToList . fmap purescriptRequire $ psm)
  liftIO $ combineManifests am js
