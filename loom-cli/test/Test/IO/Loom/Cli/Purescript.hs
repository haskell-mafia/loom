{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Cli.Purescript where

import           Control.Monad.Trans.Class (lift)

import           Loom.Cli.Process
import           Loom.Cli.Purescript

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (once)

import           X.Control.Monad.Trans.Either (eitherTFromMaybe, runEitherT)


prop_purescript =
  once . testIO . withTempDirectory "dist" "loom-ps" $ \dir -> testEitherT id $ do
    ps <- eitherTFromMaybe "Missing purescript" $ findPurescriptOnPath
    let
       f1 = dir <> "/test1.purs"
    lift $ T.writeFile f1 "module Test where\n"
    m <- firstT renderProcessError $ buildPurescript' ps [T.pack f1]
    pure $ isJust m

prop_purescript_ffi =
  once . testIO . withTempDirectory "dist" "loom-ps" $ \dir -> testEitherT id $ do
    ps <- eitherTFromMaybe "Missing purescript" $ findPurescriptOnPath
    let
       f1 = dir <> "/test1.purs"
    lift $ T.writeFile f1 . T.unlines $ [
        "module Test where"
      , "foreign import foo :: String"
      ]
    lift $ T.writeFile (dir <> "/test1.js") . T.unlines $ [
        "// module Test"
      , "\"use strict\";"
      , "exports.foo = ''"
      ]
    m <- firstT renderProcessError $ buildPurescript' ps [T.pack f1]
    pure $ isJust m

prop_purescript_fail =
  once . testIO . withTempDirectory "dist" "loom-ps" $ \dir -> testEitherT id $ do
    ps <- eitherTFromMaybe "Missing purescript" $ findPurescriptOnPath
    let
       f1 = dir <> "/test1.purs"
    lift $ T.writeFile f1 "invalid"
    m <- lift . runEitherT $ buildPurescript' ps [T.pack f1]
    pure $ isLeft m


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
