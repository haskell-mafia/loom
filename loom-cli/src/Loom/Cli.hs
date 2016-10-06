{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli (
    loom

  , LoomError (..)
  , renderLoomError
  ) where

import           Loom.Cli.Build
import           Loom.Cli.Process
import           Loom.Cli.Purescript

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, eitherTFromMaybe)


data LoomError =
    LoomProcessError ProcessError
  | LoomMissingExecutable Text
    deriving (Show)

renderLoomError :: LoomError -> Text
renderLoomError = \case
  LoomProcessError err ->
    renderProcessError err
  LoomMissingExecutable e ->
    "Could not find executable " <> e

loom :: EitherT LoomError IO ()
loom = do
  join $ loomBuild
    <$> eitherTFromMaybe (LoomMissingExecutable "purescript") findPurescriptOnPath

loomBuild :: Purescript -> EitherT LoomError IO ()
loomBuild ps = do
  _psm <- firstT LoomProcessError (withLogging "purs" $ buildPurescript ps)
  pure ()
