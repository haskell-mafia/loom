{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Handlebars (
    Templates (..)
  , buildTemplates
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Loom.Cli.Build
import           Loom.Cli.File

import           P

import           System.IO (IO, IOMode (..), hPutStrLn, withFile)


data Templates =
  Templates [FilePath]

buildTemplates :: IO Templates
buildTemplates =
  writeToFile "tmp/templateCache.js" $ \out -> do
    hbss <- findFiles (modules ["hbs"])
    withFile (T.unpack out) WriteMode $ \h -> do
      hPutStrLn h $ "var partials = {};"
      for_ hbss $ \hbs -> do
        b <- BSL.readFile (T.unpack hbs)
        let hbsName = takeDirectory hbs
        BSL.hPut h $ "partials['" <> (BSL.fromStrict . T.encodeUtf8) hbsName <> "'] = (" <> b <> ");"
      hPutStrLn h "\nmodule.exports = function(){ return partials; };"
    pure $ Templates hbss
