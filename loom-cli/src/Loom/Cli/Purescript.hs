{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Purescript (
    Purescript (..)
  , Psc (..)
  , PscBundle (..)
  , PurescriptManifest (..)
  , findPurescriptOnPath
  , buildPurescript
  , buildPurescript'
  , purescriptRequire
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Cli.Build
import           Loom.Cli.Env
import           Loom.Cli.File
import           Loom.Cli.Javascript
import           Loom.Cli.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data Purescript =
  Purescript
    Psc
    PscBundle

newtype Psc =
  Psc {
      pscPath :: FilePath
    }

newtype PscBundle =
  PscBundle {
      pscBundlePath :: FilePath
    }

data PurescriptManifest =
  PurescriptManifest {
      purescriptManifestPath :: FilePath
    , purescriptManifestFilesInput :: [FilePath]
    } deriving (Eq, Show)

findPurescriptOnPath :: IO (Maybe Purescript)
findPurescriptOnPath =
  runMaybeT $ Purescript
    <$> (Psc <$> MaybeT (verifyExecutable "psc"))
    <*> (PscBundle <$> MaybeT (verifyExecutable "psc-bundle"))

buildPurescript :: Purescript -> EitherT ProcessError IO (Maybe PurescriptManifest)
buildPurescript ps = do
  purs <- findFiles [
      "bower_components/purescript-*/src/**/*.purs"
    , "app/purs/src/**/*.purs"
    , "app/purs/test/**/*.purs"
    , "modules/**/module.purs"
    , "components/**/component.purs"
    ]
  buildPurescript' ps purs

buildPurescript' :: Purescript -> [FilePath] -> EitherT ProcessError IO (Maybe PurescriptManifest)
buildPurescript' (Purescript psc pscb) purs =
  case purs of
    [] ->
      pure Nothing
    _ ->
      writeToFile "tmp/purs/index.js" $ \out -> do
        let
          tmpOut = "app/purs/output"
          namespace = "PS"

        js <- findFiles . fmap (replaceExtension "js") $ purs

        call (pscPath psc) . mconcat $ [
            purs
          , bind (\f -> ["--ffi", f]) js
          , ["-o", tmpOut]
          ]

        -- Without the .psci file 'psci' won't work for developers
        -- FIX Just write out what we need here rather than using psci?

        -- FIX Currently a hack in bikeshed, should have a clearer definition of what are the entry points
        psModules <- liftIO $ getDirectoryContents tmpOut

        js' <- findFiles [tmpOut <> "/**/*.js"]
        call (pscBundlePath pscb) . mconcat $ [
            js'
          , bind (\m -> ["--module", m]) psModules
          , ["--namespace", namespace]
          , ["-o", out]
          ]
        liftIO . T.appendFile (T.unpack out) $ "\nmodule.exports = " <> namespace <> ";\n"

        pure . Just $ PurescriptManifest out purs

purescriptRequire :: PurescriptManifest -> BrowserifyRequire
purescriptRequire psm =
  BrowserifyRequire "ps" (purescriptManifestPath psm)
