{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Sass (
    Sass (..)
  , buildSass
  ) where

import           Loom.Cli.Asset
import           Loom.Cli.Build
import           Loom.Cli.File
import           Loom.Cli.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


newtype Sass =
  Sass {
      sassPath :: FilePath
    }

buildSass :: Sass -> AssetManifest -> EitherT ProcessError IO AssetManifest
buildSass sass am =
  writeToFile "tmp/main.css" $ \out -> do
    -- FIX Check if modified
    -- findFiles $ ["scss/**/*.scss"] <> modules ["scss"]
    call (sassPath sass) . mconcat $ [
        [out]
        -- TODO Gonna need sass globbing
        -- https://github.com/britco/node-sass-globbing/blob/master/index.js
      , ["-t", "compressed"]
      ]
    -- FIX Autoprefix "last 2 version" "ie 10"
    -- https://github.com/postcss/autoprefixer
    -- FIX replaceAssetUrls
    -- FIX Source maps
    -- FIX We need an updated asset manifest here
    pure am
