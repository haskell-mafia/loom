{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli (
    loom

  , LoomError (..)
  , renderLoomError
  ) where

import           Loom.Cli.Build
import           Loom.Cli.Purescript
import           Loom.Cli.Sass
import           Loom.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe)


data LoomError =
    LoomProcessError ProcessError
  | LoomMissingExecutable Text
  | LoomSassBuildError SassBuildError
    deriving (Show)

renderLoomError :: LoomError -> Text
renderLoomError = \case
  LoomProcessError err ->
    renderProcessError err
  LoomMissingExecutable e ->
    "Could not find executable " <> e
  LoomSassBuildError e ->
    renderSassBuildError e

loom :: EitherT LoomError IO ()
loom = do
  join $ loomBuild
    <$> eitherTFromMaybe (LoomMissingExecutable "sass") findSassOnPath
    <*> eitherTFromMaybe (LoomMissingExecutable "purescript") findPurescriptOnPath

loomBuild :: Sass -> Purescript -> EitherT LoomError IO ()
loomBuild sass ps = do
  let
    includes =
      SassIncludes "scss/main.scss" [".", "scss"]
  _bas <- firstT LoomSassBuildError . withLogging "css" $ buildSass sass includes
  _psm <- firstT LoomProcessError (withLogging "purs" $ buildPurescript ps)
  pure ()
