{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Cli.Sass where

import           Control.Monad.Trans.Class (lift)

import           Loom.Cli.Sass

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.Directory (doesFileExist)
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck ((===), once)

import           X.Control.Monad.Trans.Either (eitherTFromMaybe, runEitherT)


prop_sass =
  once . testIO . withTempDirectory "dist" "loom-sass" $ \dir -> testEitherT id $ do
    ps <- eitherTFromMaybe "Missing sass" $ findSassOnPath
    let
       f1 = dir <> "/test1.scss"
       f2 = dir <> "/test2.scss"
    lift $ T.writeFile f1 "$test: #ffffff;"
    lift $ T.writeFile f2 "@import \"test1.scss\";\n .foo { color: $test; }"
    let
      si = SassIncludes (T.pack f2) [T.pack dir]
    m <- firstT renderSassError $ buildSass ps si
    lift . fmap (=== Just True) $ mapM (doesFileExist . T.unpack) m

prop_sass_missing =
  once . testIO $ testEitherT id $ do
    ps <- eitherTFromMaybe "Missing sass" $ findSassOnPath
    let
      si = SassIncludes (T.pack "missing.scss") []
    m <- firstT renderSassError $ buildSass ps si
    pure $ m === Nothing

prop_sass_fail =
  once . testIO . withTempDirectory "dist" "loom-sass" $ \dir -> testEitherT id $ do
    ps <- eitherTFromMaybe "Missing sass" $ findSassOnPath
    let
       f1 = dir <> "/test1.scss"
    lift $ T.writeFile f1 ".foo { color: $test; }"
    let
      si = SassIncludes (T.pack f1) [T.pack dir]
    m <- lift . runEitherT $ buildSass ps si
    pure $ isLeft m


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
